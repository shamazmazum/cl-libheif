(in-package :cl-libheif)

(define-foreign-library libheif
  (:unix (:or "libheif.so.1" "libheif.so"))
  (t (:default "libheif")))
(use-foreign-library libheif)

(defmacro defwrapper (name)
  "Make a new type for a pointer"
  `(serapeum:defconstructor ,name
     (obj t)))

(defwrapper init-parameters)

;; Currently no parameters are supported
(defparameter +default-init-parameters+
  (init-parameters (null-pointer))
  "Default libheif init parameters.")

(defcfun (%init "heif_init") (:struct heif-error)
  (parameters :pointer))

(serapeum:-> init (init-parameters) (values &optional))
(defun init (parameters)
  (let ((result (%init (init-parameters-obj parameters))))
    (analyse-error result))
  (values))

(defcfun (deinit "heif_deinit") :void)

(defmacro with-libheif ((parameters) &body body)
  "Initialize libheif and execute @c(body). Libheif is deinitialized
when the control leaves @c(body)."
  `(progn
     (init ,parameters)
     (unwind-protect (progn ,@body)
       (deinit))))
