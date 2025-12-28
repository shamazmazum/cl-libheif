(in-package :cl-libheif)

;; Decoder Descriptors
(defwrapper decoder-descriptor)

(defcfun (%decoder-descriptors "heif_get_decoder_descriptors") :int
  (format-filter heif-compression-format)
  (decoders      :pointer)
  (count         :int))

(serapeum:-> decoder-descriptors (&optional compression-format)
             (values list &optional))
(defun decoder-descriptors (&optional (filter :undefined))
  (let ((count (%decoder-descriptors filter (null-pointer) 0)))
    (with-foreign-object (decoders-ptr :pointer count)
      (let ((n (%decoder-descriptors filter decoders-ptr count)))
        (assert (= count n))
        (loop for i below count collect
              (decoder-descriptor (mem-aref decoders-ptr :pointer i)))))))

(defcfun (%decoder-descriptor-name "heif_decoder_descriptor_get_name") :string
  (descriptor :pointer))

(serapeum:-> decoder-descriptor-name (decoder-descriptor)
             (values string &optional))
(defun decoder-descriptor-name (descriptor)
  (%decoder-descriptor-name
   (decoder-descriptor-obj descriptor)))

(defcfun (%decoder-descriptor-id-name "heif_decoder_descriptor_get_id_name") :string
  (descriptor :pointer))

(serapeum:-> decoder-descriptor-id-name (decoder-descriptor)
             (values string &optional))
(defun decoder-descriptor-id-name (descriptor)
  (%decoder-descriptor-id-name
   (decoder-descriptor-obj descriptor)))

;; Decoding
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
