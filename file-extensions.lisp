(in-package #:media-types)

(def mime.types
  (asdf:system-relative-pathname :media-types "mime.types")
  "Where to store Apache's mime.types file.")

(defun dotted-extension? (x)
  (and (stringp x) (string^= "." x)))

(deftype dotted-extension ()
  '(and string (satisfies dotted-extension?)))

(deftype bare-extension ()
  '(and string (not dotted-extension)))

(deftype no-extension ()
  'null)

(deftype extension ()
  '(or dotted-extension bare-extension no-extension))

(deftype file-designator ()
  '(or string pathname))

(deftype type-extensions ()
  '(or string list))

(defun comment? (line)
  (and (stringp line) (string^= "#" line)))

(defun snarf-mime.types ()
  (let ((string (read-file-into-string mime.types)))
    (loop for line in (lines string)
          unless (or (emptyp line)
                     (comment? line))
            collect (tokens line))))

(defun make-type-to-extension-table ()
  (loop with table = (make-hash-table :test 'equalp)
        for (type . exts) in (snarf-mime.types)
        do (setf (gethash type table)
                 (if (single exts)
                     (car exts)
                     exts))
        finally (return table)))

(defun make-extension-to-type-table ()
  (loop with table = (make-hash-table :test 'equalp)
        for (type . exts) in (snarf-mime.types)
        do (dolist (ext exts)
             (setf (gethash ext table) type))
        finally (return table)))

(defparameter *media-type-extensions*
  (make-type-to-extension-table))

(defparameter *extension-media-types*
  (make-extension-to-type-table))

(defun media-type-extension (type)
  (let ((extensions (gethash type *media-type-extensions*)))
    (etypecase-of type-extensions extensions
      (string extensions)
      (list (values-list extensions)))))

(defun extension-media-type (extension)
  (etypecase-of extension extension
    (dotted-extension
     (extension-media-type
      (nsubseq extension 1)))
    (bare-extension
     (values (gethash extension *extension-media-types*
                      "application/octet-stream")))
    (no-extension
     "application/octet-stream")))

(defun file-name-extension (file)
  (etypecase-of file-designator file
    (string (file-name-extension (pathname file)))
    (pathname (pathname-type file))))

(defun file-name-media-type (file)
  (extension-media-type (file-name-extension file)))

(defun file-name-media-typep (file type)
  (media-subtypep (file-name-media-type file) type))

(define-compiler-macro file-name-media-typep (&whole call file type
                                                     &environment env)
  (if (constantp type env)
      `(file-media-media-typep ,file (load-time-value (parse-media-type ,type)))
      call))
