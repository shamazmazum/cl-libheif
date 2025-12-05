(defpackage cwrapper
  (:use #:cl))
(in-package :cwrapper)

(defclass asdf::cwrapper (asdf:source-file)
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

(defmethod asdf:output-files ((operation asdf:compile-op) (component asdf::cwrapper))
  (values (list (asdf:apply-output-translations
                 (make-pathname :name (pathname-name (asdf:component-pathname component))
                                :type (dynamic-library-extension)
                                :defaults (asdf:component-pathname component))))
          t))

(defmethod asdf:perform ((operation asdf:load-op) (component asdf::cwrapper))
  t)

(defmethod asdf:perform ((operation asdf:compile-op) (component asdf::cwrapper))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (asdf:component-pathname component))
           (shared-object (first (asdf:output-files operation component))))
      (ensure-directories-exist shared-object)
      ;; TODO: Discover all needed flags
      (uiop:run-program
       (list "cc" "-fPIC" "-shared"
             #+freebsd
             "-I/usr/local/include"
             "-o" (nn shared-object) (nn c-file))))))
