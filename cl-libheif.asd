(defsystem :cl-libheif
    :description "A wrapper around libheif"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.1"
    :depends-on (:cffi :cffi-libffi :serapeum)
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "libheif")))
