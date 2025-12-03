(defsystem :cl-libheif
    :description "A wrapper around libheif"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.1"
    :depends-on (:cffi :cffi-libffi :serapeum)
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
                 (:file "exif")))
