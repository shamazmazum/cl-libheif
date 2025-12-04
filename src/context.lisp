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

(defcfun (%context-number-of-top-level-images "heif_context_get_number_of_top_level_images")
    :int
  (context :pointer))

(defcfun (%context-top-level-image-ids "heif_context_get_list_of_top_level_image_IDs")
    :int
  (context :pointer)
  (ids     (:pointer :uint32))
  (count   :int))

(serapeum:-> context-top-level-image-ids (context &optional (integer 0))
             (values list &optional))
(defun context-top-level-image-ids (context &optional (max-ids 0))
  "Return a list of toplevel image ids. If MAX-IDS > 0, return no more than MAX-IDS."
  (let* ((n (%context-number-of-top-level-images
             (context-obj context)))
         (n (if (zerop max-ids) n (min n max-ids))))
    (with-foreign-object (ids-ptr :uint32 n)
      (let ((nreceived (%context-top-level-image-ids
                        (context-obj context)
                        ids-ptr n)))
        (assert (= n nreceived)))
      (loop for i below n collect
            (mem-aref ids-ptr :uint32 i)))))

(defcfun (%context-primary-image-id "heif_context_get_primary_image_ID")
    (:struct heif-error)
  (context :pointer)
  (id      (:pointer :uint32)))

(serapeum:-> context-primary-image-id (context) (values integer &optional))
(defun context-primary-image-id (context)
  (with-foreign-object (id-ptr :uint32)
    (let ((result (%context-primary-image-id
                   (context-obj context) id-ptr)))
      (analyse-error result))
    (mem-ref id-ptr :uint32)))
