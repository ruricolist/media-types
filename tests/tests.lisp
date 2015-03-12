(in-package #:media-types.tests)

(def-suite media-types)
(in-suite media-types)

(defun run-tests ()
  (5am:run! 'media-types))

(test wildcard-extension
  (is (null (file-name-extension "index.php?"))))

(test extension-media-type
  (is (equal (extension-media-type "txt") "text/plain"))
  (is (equal (extension-media-type "TXT") "text/plain"))
  (is (equal (extension-media-type ".txt") "text/plain"))
  (is (equal (extension-media-type "my-fake-extension") "application/octet-stream"))
  (is (equal (extension-media-type nil) "application/octet-stream")))

(test media-type-extension
  (is (equal (media-type-extension "application/javascript") "js"))
  (is (equal (multiple-value-list
              (media-type-extension "image/jpeg"))
             '("jpeg" "jpg" "jpe"))))

(test file-name-extension
  (is (equal (file-name-extension "file.txt") "txt")))

(test file-name-media-type
  (is (equal (file-name-media-type "file.txt") "text/plain"))
  (is (equal (file-name-media-type #p"file.txt") "text/plain"))
  (is (equal (file-name-media-type "file") "application/octet-stream")))

(test file-name-media-typep
  (is (file-name-media-typep "file.xml" "application/xml"))
  (is (file-name-media-typep "file.svg" "application/xml")))

(test media-subtypep
  (is (media-subtypep "text/plain" "*/*"))
  (is (media-subtypep "text/plain" "text/*"))
  (is (media-subtypep "text/plain" "application/octet-stream"))
  (is (media-subtypep "image/svg+xml" "application/xml"))
  (is (media-subtypep "application/vnd.my-format+json" "application/json"))
  (is (media-subtypep "application/atom+xml;type=entry" "application/xml"))
  (is (not (media-subtypep "application/atom+xml" "application/atom+xml;type=entry")))
  (is (media-subtypep "application/atom+xml;charset=UTF8;type=entry"
                      "application/atom+xml;type=entry"))
  (is (media-subtypep "application/json" "*/*;q=0.1"))

  (is (media-subtypep "text/html;charset=UTF8" "text/html"))
  (is (media-subtypep "application/json" "*/*;q=0.1"))
  (is (media-subtypep "text/html;charset=UTF8" "text/*"))
  (is (media-subtypep "text/html;charset=UTF8" "application/octet-stream"))
  (is (not (media-subtypep "application/octet-stream" "text/html;charset=UTF8")))
  (is (not (media-subtypep "text/html" "text/html;charset=UTF8")))
  (is (media-subtypep "application/atom+xml" "application/xml"))
  (is (media-subtypep "application/atom+xml;type=entry" "application/xml"))
  (is (media-subtypep "application/atom+xml;charset=UTF8;type=entry"
                          "application/atom+xml;type=entry"))
  (is (not (media-subtypep "application/atom+xml" "application/atom+xml;type=entry")))
  (is (media-subtypep "application/atom+xml;type=entry" "application/atom+xml"))
  (is (not (media-subtypep "application/atom+xml" "application/atom+xml;type=entry")))
  (is (media-subtypep "application/foo+fastinfoset" "application/fastinfoset"))
  (is (media-subtypep "application/foo+wbxml" "application/vnd.wap.wbxml"))
  (is (media-subtypep "application/json" 'application/json))
  (is (media-subtypep "image/svg+xml" "application/xml")))
