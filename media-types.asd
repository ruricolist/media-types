;;;; media-types.asd

(asdf:defsystem #:media-types
  :description "Query and compare media types."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria #:serapeum #:cl-ppcre)
  :in-order-to ((asdf:test-op (asdf:test-op #:media-types-tests)))
  :serial t
  :components ((:file "package")
               (:file "media-types")
               (:static-file "mime.types")
               (:file "file-extensions")))

(asdf:defsystem #:media-types-tests
  :description "Test suite for media-types."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:media-types #:fiveam)
  :perform (asdf:test-op (o c) (uiop:symbol-call :media-types.tests :run-tests))
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")))
