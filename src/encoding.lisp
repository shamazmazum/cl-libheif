(in-package :cl-libheif)

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
  "Get an encoder for the specified format and execute BODY in its
scope. Ensure that the encoder is released when the control leaves
BODY."
  `(let ((,encoder (get-encoder-for-format ,context ,format)))
     (unwind-protect
          (progn ,@body)
       (encoder-release ,encoder))))

(defcfun (%encoder-set-lossy-quality! "heif_encoder_set_lossy_quality")
    (:struct heif-error)
  (encoder :pointer)
  (quality :int))

(serapeum:-> encoder-set-lossy-quality! (encoder integer) (values &optional))
(defun encoder-set-lossy-quality! (encoder quality)
  (let ((result (%encoder-set-lossy-quality! (encoder-obj encoder) quality)))
    (analyse-error result))
  (values))


(defcfun (%context-encode-image "heif_context_encode_image")
    (:struct heif-error)
  (context :pointer)
  (image   :pointer)
  (encoder :pointer)
  (options :pointer)
  (handle  :pointer))

(serapeum:-> context-encode-image (context image encoder encoding-options) (values &optional))
(defun context-encode-image (context image encoder options)
  (let ((result (%context-encode-image
                 (context-obj          context)
                 (image-obj            image)
                 (encoder-obj          encoder)
                 (encoding-options-obj options)
                 (null-pointer))))
    (analyse-error result))
  (values))
