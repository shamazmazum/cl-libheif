(defsystem :cl-libheif
    :description "A wrapper around libheif"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.1"
    :depends-on (:cffi :cffi-libffi :serapeum :trivial-octet-streams)
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "library")
                 (:file "conditions")
                 (:file "enums")
                 (:file "context")
                 (:file "io")
                 (:file "image-handle")
                 (:file "image")
                 (:file "decoding")
                 (:file "encoding")
                 (:file "exif"))
    :in-order-to ((test-op (load-op "cl-libheif/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :cl-libheif/tests))))))

(defsystem :cl-libheif/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:cl-libheif :fiveam :cl-value-noise :float-features))
