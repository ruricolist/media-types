;;;; package.lisp

(defpackage #:media-types
  (:use #:cl #:alexandria #:serapeum)
  (:export :media-subtypep
           :media-type=
           :media-type-supers
           :parse-media-type
           :media-type-values
           :media-type
           :invalid-media-type

           :media-type-extension
           :extension-media-type
           :file-name-extension
           :file-name-media-type
           :pathname-media-type
           :file-name-media-typep))
