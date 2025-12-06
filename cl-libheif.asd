(defclass cwrapper (source-file)
  ()
  (:default-initargs
   :type "c"))

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))

(defmethod output-files ((operation compile-op) (component cwrapper))
  (values (list (apply-output-translations
                 (make-pathname :name (pathname-name (component-pathname component))
                                :type (dynamic-library-extension)
                                :defaults (component-pathname component))))
          t))

(defmethod perform ((operation load-op) (component cwrapper))
  t)

(defmethod perform ((operation compile-op) (component cwrapper))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (component-pathname component))
           (shared-object (first (output-files operation component))))
      (ensure-directories-exist shared-object)
      ;; TODO: Discover all needed flags
      (uiop:run-program
       (list "cc" "-fPIC" "-shared"
             #+freebsd
             "-I/usr/local/include"
             "-o" (nn shared-object) (nn c-file))))))


(defsystem :cl-libheif
    :description "A wrapper around libheif"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.1"
    :depends-on (:cffi :cffi-libffi :serapeum :trivial-octet-streams)
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:cwrapper "tiny-wrapper")
                 (:file "conditions")
                 (:file "library")
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
