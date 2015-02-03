(in-package #:media-types)

(defstruct media-type
  (type (error "No type") :type string :read-only t)
  (subtype (error "No subtype") :type string :read-only t)
  (params nil :type list :read-only t))

(defun media-type (type subtype &rest params)
  (make-media-type :type type
                   :subtype subtype
                   :params params))

(deftype media-type-designator ()
  '(or media-type string symbol))

(defcondition invalid-media-type (error)
  ((type :initarg :type))
  (:report (lambda (c s)
             (with-slots (type) c
               (format s "Invalid media type: ~a" type)))))

(def media-ranges
  '(:application :audio :image :message :model :multipart :text :video))

;; Constant types.

(def application/octet-stream
  (make-media-type :type "application" :subtype "octet-stream"))

(def application/xml
  (make-media-type :type "application" :subtype "xml"))

(def application/json
  (make-media-type :type "application" :subtype "json"))

(def application/x-www-form-urlencoded
  (make-media-type :type "application" :subtype "x-www-form-urlencoded"))

(def multipart/form-data
  (make-media-type :type "multipart" :subtype "form-data"))

(def application/zip
  (make-media-type :type "application" :subtype "zip"))

(def application/cbor
  (make-media-type :type "application" :subtype "cbor"))

(def application/fastinfoset
  (make-media-type :type "application" :subtype "fastinfoset"))

(def application/vnd.wap.wbxml
  (make-media-type :type "application" :subtype "vnd.wap.wbxml"))

;; https://www.iana.org/assignments/media-type-structured-suffix/media-type-structured-suffix.xml
(def suffix-types
  '(("xml" . "application/xml")
    ("json" . "application/json")
    ("cbor" . "application/cbor")
    ("fastinfoset" . "application/fastinfoset")
    ("wbxml" . "application/vnd.wap.wbxml")
    ("zip" . "application/zip")))

(defun suffix-type (suffix)
  (assoc-value suffix-types suffix :test #'equal))

(defgeneric media-subtypep (subtype type))

(defun render-media-type (self stream)
  (format stream
          "~a/~a~@[;~:{~a=~a~^;~}~]"
          (media-type-type self)
          (media-type-subtype self)
          (media-type-params self)))

(defmethod print-object ((self media-type) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :type t :identity nil)
        (render-media-type self stream))
      (render-media-type self stream)))

(defun media-type-values (type)
  (let ((type (parse-media-type type)))
    (values (media-type-type type)
            (media-type-subtype type)
            (media-type-params type))))

(defun media-type= (x y)
  (let ((x (parse-media-type x))
        (y (parse-media-type y)))
    (or (equalp x y)
        (and (media-subtypep x y)
             (media-subtypep y x)))))

(defun parse-media-type (type &key (allow-params t))
  (etypecase-of media-type-designator type
    (media-type type)
    (symbol (parse-media-type (string-downcase type) :allow-params allow-params))
    (string (parse-media-type-string type :allow-params allow-params))))

(defun parse-media-type-string (type &key (allow-params t))
  (check-type type string)
  (nth-value-or 1
    (let ((parts (ppcre:split "(\\s|[/;= ])+" type)))
      (when (>= (length parts) 2)
        (destructuring-bind (type subtype &rest params)
            parts
          (let* ((params (plist-alist params))
                 (q (cdr (pop-assoc "q" params :test #'string-equal)))
                 (q (ensure-q type subtype q))
                 (type (make-media-type :type type :subtype subtype :params params)))
            (check-media-type-valid type :allow-params allow-params)
            (values type q)))))
    (values application/octet-stream 1)))

(defun ensure-q (type subtype q)
  (if q
      (if-let (float (ignore-errors (parse-float q)))
        (clamp float 0 1)
        1)
      (if (equal subtype "*")
          (if (equal type "*")
              0.01
              0.02)
          1)))

(defun media-type-valid (type &key allow-params)
  (multiple-value-bind (type subtype params)
      (media-type-values type)
    (or (and (equal type "*")
             (equal subtype "*"))
        (and (or (equal type "*")
                 (find type media-ranges :test #'string-equal))
             (or allow-params
                 (null params))))))

(defun check-media-type-valid (type &key allow-params)
  (unless (media-type-valid type :allow-params allow-params)
    (error 'invalid-media-type :type type)))

(defmethod media-subtypep (subtype type)
  (media-subtypep (parse-media-type subtype)
                  (parse-media-type type)))

(defun extract-suffix (subtype)
  (check-type subtype string)
  (when-let (pos (position #\+ subtype :from-end t))
    (subseq subtype (1+ pos))))

(defmethod media-subtypep ((subtype media-type) (type media-type))
  (mvlet ((type1 subtype1 params1 (media-type-values subtype))
          (type2 subtype2 params2 (media-type-values type)))
    (or (and (equal type2 "*")
             (equal subtype2 "*"))
        (and (equal type2 "application")
             (equal subtype2 "octet-stream"))
        (when-let* ((suffix (extract-suffix subtype1))
                    (subtype (suffix-type suffix)))
          (media-subtypep (suffix-type suffix) type))
        (and (equal type1 type2)
             (or (equal subtype2 "*")
                 (equal subtype1 subtype2))
             (subsetp params2 params1 :test #'equalp)))))

(define-compiler-macro media-subtypep (&whole decline
                                              subtype type
                                              &environment env)
  (cond ((and (constantp subtype env)
              (constantp type env))
         `(media-subtypep (load-time-value (parse-media-type ,subtype))
                          (load-time-value (parse-media-type ,type))))
        ((constantp subtype env)
         `(media-subtypep (load-time-value (parse-media-type ,subtype))
                          ,type))
        ((constantp type env)
         `(media-subtypep ,subtype
                          (load-time-value (parse-media-type ,type))))
        (t decline)))

(defmethod media-type-supers ((type media-type))
  (let ((supers '()))
    (multiple-value-bind (type subtype params)
        (media-type-values type)
      (push (parse-media-type (concat type "/*")) supers)
      (when-let (pos (position #\+ subtype))
        (let ((ext (subseq subtype (min (1+ pos) (length subtype)))))
          (string-case (string-downcase ext)
            ("xml" (push application/xml supers))
            ("json" (push application/json supers)))))
      (when params (loop for set in (sort (powerset params) #'< :key #'length)
                         if (< (length set) (length params))
                           do (push (make-media-type :type type
                                                     :subtype subtype
                                                     :params set)
                                    supers))))
    ;; Already in descending order of specificity.
    (delete-duplicates supers :test #'media-type=)))

(defmethod media-type-supers ((type string))
  (media-type-supers (parse-media-type type)))

(defmethod media-type-supers ((type symbol))
  (media-type-supers (parse-media-type type)))
