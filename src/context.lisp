;; Context creation and manipulation + library init/deinit

(in-package :cl-libheif)

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
  "Bind @c(context) to a newly created libheif context and execute
@c(body) in its scope. The context is destroyed when the control
leaves @c(body)."
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
  "Return a list of toplevel image ids. If @c(max-ids) > 0, return no
more than @c(max-ids)."
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
  "Get ID of a primary toplevel image."
  (with-foreign-object (id-ptr :uint32)
    (let ((result (%context-primary-image-id
                   (context-obj context) id-ptr)))
      (analyse-error result))
    (mem-ref id-ptr :uint32)))

(defcfun (%context-set-maximum-image-size-limit! "heif_context_set_maximum_image_size_limit")
    :void
  (context :pointer)
  (width   :int))

(serapeum:-> context-set-maximum-image-size-limit! (context (integer 0)) (values &optional))
(defun context-set-maximum-image-size-limit! (context width)
  "Set the maximum image size security limit. This function will set
the maximum image area (number of pixels) to @c((expt width 2))."
  (%context-set-maximum-image-size-limit! (context-obj context) width)
  (values))

(defcfun (%context-set-max-decoding-threads! "heif_context_set_max_decoding_threads") :void
  (context :pointer)
  (threads :int))

(serapeum:-> context-set-max-decoding-threads! (context (integer 0)) (values &optional))
(defun context-set-max-decoding-threads! (context nthreads)
  "Set the maximal number of threads used by libheif. Setting this to
0 means decoding in the main thread, while 1 means that a separate
thread dedicated to decoding is created."
  (%context-set-max-decoding-threads! (context-obj context) nthreads)
  (values))
