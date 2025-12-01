;; Context creation and manipulation + library init/deinit

(in-package :cl-libheif)

(defwrapper init-parameters)

;; Currently no parameters are supported
(defparameter +default-init-parameters+
  (init-parameters (null-pointer)))

(defcfun (%init "heif_init") (:struct heif-error)
  (parameters :pointer))

(serapeum:-> init (init-parameters) (values &optional))
(defun init (parameters)
  (let ((result (%init (init-parameters-obj parameters))))
    (analyse-error result))
  (values))

(defcfun (deinit "heif_deinit") :void)

(defmacro with-libheif ((parameters) &body body)
  "Execute BODY with libheif being initialized. It is deinitialized
when the control leaves BODY."
  `(progn
     (init ,parameters)
     (unwind-protect (progn ,@body)
       (deinit))))

(defwrapper context)

(defcfun (%alloc-context "heif_context_alloc") :pointer)

(serapeum:-> alloc-context () (values context &optional))
(defun alloc-context ()
  "Allocate libheif context"
  (context (%alloc-context)))

(defcfun (%free-context "heif_context_free") :void
  (obj :pointer))

(serapeum:-> free-context (context) (values &optional))
(defun free-context (context)
  "Free libheif context"
  (%free-context
   (context-obj context))
  (values))

(defmacro with-context ((context) &body body)
  "Execute BODY in libheif context. The context is destroyed when the
control leaves BODY."
  `(let ((,context (alloc-context)))
     (unwind-protect
          (progn ,@body)
       (free-context ,context))))
