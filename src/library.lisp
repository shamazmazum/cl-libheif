(in-package :cl-libheif)

(define-foreign-library libheif
  (:unix (:or "libheif.so.1" "libheif.so"))
  (t (:default "libheif")))
(use-foreign-library libheif)

(defmacro defwrapper (name)
  "Make a new type for a pointer"
  `(serapeum:defconstructor ,name
     (obj t)))
