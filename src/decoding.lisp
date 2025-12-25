(in-package :cl-libheif)

(defwrapper decoding-options)

(defparameter +default-decoding-options+
  (decoding-options (null-pointer))
  "Default decoding options")

(defcfun (%decode-image "heif_decode_image") (:struct heif-error)
  (handle     :pointer)
  (image      :pointer)
  (colorspace heif-colorspace)
  (chroma     heif-chroma)
  (options    :pointer))

(serapeum:-> decode-image (image-handle colorspace chroma decoding-options)
             (values image &optional))
(defun decode-image (handle colorspace chroma options)
  (with-foreign-object (image-ptr :pointer)
    (let ((result (%decode-image
                   (image-handle-obj handle)
                   image-ptr colorspace chroma
                   (decoding-options-obj options))))
      (analyse-error result))
    (image (mem-ref image-ptr :pointer))))

(defmacro with-decode-image ((image handle colorspace chroma options) &body body)
  "Bind @c(image) to a decoded image and execute @c(body) in the scope
of that image. Make sure the image is released when the control leaves
@c(body)."
  `(let ((,image (decode-image ,handle ,colorspace ,chroma ,options)))
     (unwind-protect
          (progn ,@body)
       (image-release ,image))))
