(in-package :cl-libheif)

;; :boolean type is broken for some reason

(defmethod translate-into-foreign-memory (value (type cffi::foreign-type-wrapper) ptr)
  (translate-into-foreign-memory
   (funcall (cffi::wrapper-to-c type) value)
   (cffi::actual-type type) ptr))

(defun bool-c-to-lisp (value)
  (not (zerop value)))

(defun bool-lisp-to-c (value)
  (if value 1 0))

(defctype heif-boolean (:wrapper :int :from-c bool-c-to-lisp
                                 :to-c bool-lisp-to-c))

(defwrapper encoding-options)
(defparameter +default-encoding-options+
  (encoding-options (null-pointer))
  "Default encoding options")

(defwrapper encoder)

(defcfun (%get-encoder-for-format "heif_context_get_encoder_for_format")
    (:struct heif-error)
  (context :pointer)
  (format  heif-compression-format)
  (encoder :pointer))

(serapeum:-> get-encoder-for-format (context compression-format)
             (values encoder &optional))
(defun get-encoder-for-format (context format)
  (with-foreign-object (encoder-ptr :pointer)
    (let ((result (%get-encoder-for-format
                   (context-obj context)
                   format encoder-ptr)))
      (analyse-error result))
    (encoder (mem-ref encoder-ptr :pointer))))

(defcfun (%encoder-release "heif_encoder_release") :void
  (encoder :pointer))

(serapeum:-> encoder-release (encoder) (values &optional))
(defun encoder-release (encoder)
  (%encoder-release (encoder-obj encoder))
  (values))

(defmacro with-encoder-for-format ((encoder context format) &body body)
  "Bind @c(encoder) to an encoder for the specified format and execute
@c(body) in its scope. Ensure that the encoder is released when the
control leaves @c(body)."
  `(let ((,encoder (get-encoder-for-format ,context ,format)))
     (unwind-protect
          (progn ,@body)
       (encoder-release ,encoder))))

(defcfun (%encoder-set-lossy-quality! "heif_encoder_set_lossy_quality")
    (:struct heif-error)
  (encoder :pointer)
  (quality :int))

(serapeum:-> encoder-set-lossy-quality! (encoder (integer 0 100)) (values &optional))
(defun encoder-set-lossy-quality! (encoder quality)
  "Set a 'quality' factor (0-100). How this is mapped to actual
encoding parameters is encoder dependent."
  (let ((result (%encoder-set-lossy-quality! (encoder-obj encoder) quality)))
    (analyse-error result))
  (values))

(defcfun (%encoder-set-lossless! "heif_encoder_set_lossless")
    (:struct heif-error)
  (encoder :pointer)
  (enable  heif-boolean))

(serapeum:-> encoder-set-lossless! (encoder boolean) (values &optional))
(defun encoder-set-lossless! (encoder enablep)
  "Enable or disable lossless encoding."
  (let ((result (%encoder-set-lossless! (encoder-obj encoder) enablep)))
    (analyse-error result))
  (values))

(defcfun (%context-encode-image! "heif_context_encode_image")
    (:struct heif-error)
  (context :pointer)
  (image   :pointer)
  (encoder :pointer)
  (options :pointer)
  (handle  :pointer))

(serapeum:-> context-encode-image!
             (context image encoder encoding-options)
             (values &optional))
(defun context-encode-image! (context image encoder options)
  "Compress the input image. The first compressed image becomes the
primary image in that context."
  (let ((result (%context-encode-image!
                 (context-obj          context)
                 (image-obj            image)
                 (encoder-obj          encoder)
                 (encoding-options-obj options)
                 (null-pointer))))
    (analyse-error result))
  (values))

(defwrapper encoder-descriptor)

(defcfun (%encoder-descriptors "heif_get_encoder_descriptors") :int
  (format-filter heif-compression-format)
  (name-filter   :string)
  (encoders      :pointer)
  (count         :int))

(serapeum:-> encoder-descriptors
             (&optional compression-format (or string null))
             (values list &optional))
(defun encoder-descriptors (&optional (format-filter :undefined) name-filter)
  "Get a list of encoder descriptors supported by libheif."
  (let* ((name-filter (if name-filter name-filter (null-pointer)))
         (n (%encoder-descriptors format-filter name-filter (null-pointer) 0)))
    (with-foreign-object (encoders-ptr :pointer n)
      (let ((received-n (%encoder-descriptors format-filter name-filter encoders-ptr n)))
        (assert (= received-n n))
        (loop for i below n collect
              (encoder-descriptor
               (mem-aref encoders-ptr :pointer i)))))))

(defcfun (%encoder-descriptor-name "heif_encoder_descriptor_get_name") :string
  (descriptor :pointer))

(serapeum:-> encoder-descriptor-name (encoder-descriptor) (values string &optional))
(defun encoder-descriptor-name (descriptor)
  "Return a long, descriptive name of the encoder (including version
information)."
  (%encoder-descriptor-name (encoder-descriptor-obj descriptor)))

(defcfun (%encoder-descriptor-id-name "heif_encoder_descriptor_get_id_name") :string
  (descriptor :pointer))

(serapeum:-> encoder-descriptor-id-name (encoder-descriptor) (values string &optional))
(defun encoder-descriptor-id-name (descriptor)
  "Return a short, symbolic name for identifying the encoder. This
name should stay constant over different encoder versions."
  (%encoder-descriptor-id-name (encoder-descriptor-obj descriptor)))

(defcfun (%encoder-descriptor-compression-format
          "heif_encoder_descriptor_get_compression_format")
    heif-compression-format
  (descriptor :pointer))

(serapeum:-> encoder-descriptor-compression-format
             (encoder-descriptor)
             (values compression-format &optional))
(defun encoder-descriptor-compression-format (descriptor)
  "Get compression format for this encoder."
  (%encoder-descriptor-compression-format (encoder-descriptor-obj descriptor)))

(defcfun (%encoder-descriptor-supports-lossy-compression-p
          "heif_encoder_descriptor_supports_lossy_compression")
    :boolean
  (descriptor :pointer))

(serapeum:-> encoder-descriptor-supports-lossy-compression-p
             (encoder-descriptor)
             (values boolean &optional))
(defun encoder-descriptor-supports-lossy-compression-p (descriptor)
  "Return @c(t) if this encoder supports lossy compression, @c(nil)
oterwise."
  (%encoder-descriptor-supports-lossy-compression-p
   (encoder-descriptor-obj descriptor)))

(defcfun (%encoder-descriptor-supports-lossless-compression-p
          "heif_encoder_descriptor_supports_lossless_compression")
    :boolean
  (descriptor :pointer))

(serapeum:-> encoder-descriptor-supports-lossless-compression-p
             (encoder-descriptor)
             (values boolean &optional))
(defun encoder-descriptor-supports-lossless-compression-p (descriptor)
  "Return @c(t) if this encoder supports lossless compression, @c(nil)
oterwise."
  (%encoder-descriptor-supports-lossless-compression-p
   (encoder-descriptor-obj descriptor)))

(defcfun (%context-encoder "heif_context_get_encoder") (:struct heif-error)
  (context    :pointer)
  (descriptor :pointer)
  (encoder    :pointer))

(serapeum:-> context-encoder (context encoder-descriptor)
             (values encoder &optional))
(defun context-encoder (context descriptor)
  (with-foreign-object (encoder-ptr :pointer)
    (let ((result (%context-encoder
                   (context-obj context)
                   (encoder-descriptor-obj descriptor)
                   encoder-ptr)))
      (analyse-error result))
    (encoder (mem-ref encoder-ptr :pointer))))

(defmacro with-encoder ((encoder context descriptor) &body body)
  "Bind @c(encoder) to a reference to the encoder by its descriptor
and execute @c(body) in its scope. Make sure it is released when the
control leaves @c(body)."
  `(let ((,encoder (context-encoder ,context ,descriptor)))
     (unwind-protect
          (progn ,@body)
       (encoder-release ,encoder))))
