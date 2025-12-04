(in-package :cl-libheif)

;; Image handle
(defwrapper image-handle)

(defcfun (%get-primary-image-handle "heif_context_get_primary_image_handle")
    (:struct heif-error)
  (context :pointer)
  (handle  :pointer))

(serapeum:-> get-primary-image-handle (context) (values image-handle &optional))
(defun get-primary-image-handle (context)
  (with-foreign-object (handle-ptr :pointer)
    (let ((result (%get-primary-image-handle
                   (context-obj context)
                   handle-ptr)))
      (analyse-error result))
    (image-handle
     (mem-ref handle-ptr :pointer))))

(defcfun (%release-image-handle "heif_image_handle_release") :void
  (handle :pointer))

(serapeum:-> release-image-handle (image-handle) (values &optional))
(defun release-image-handle (handle)
  (%release-image-handle
   (image-handle-obj handle)))

(defcfun (%image-handle-width "heif_image_handle_get_width") :int
  (handle :pointer))

(serapeum:-> image-handle-width (image-handle)
             (values integer &optional)) ; Can it be negative?
(defun image-handle-width (handle)
  "Get width of the image"
  (%image-handle-width
   (image-handle-obj handle)))


(defcfun (%image-handle-height "heif_image_handle_get_height") :int
  (handle :pointer))

(serapeum:-> image-handle-height (image-handle)
             (values integer &optional)) ; Ditto
(defun image-handle-height (handle)
  "Get height of the image"
  (%image-handle-height
   (image-handle-obj handle)))


(defcfun (%image-handle-preferred-decoding-colorspace
          "heif_image_handle_get_preferred_decoding_colorspace")
    (:struct heif-error)
  (handle     :pointer)
  (colorspace (:pointer heif-colorspace))
  (chroma     (:pointer heif-chroma)))

(serapeum:-> image-handle-preferred-decoding-colorspace
             (image-handle)
             (values colorspace chroma &optional))
(defun image-handle-preferred-decoding-colorspace (handle)
  "Get the colorspace and chroma parameters which are preferred when
decoding the image."
  (with-foreign-objects ((colorspace-ptr 'heif-colorspace)
                         (chroma-ptr     'heif-chroma))
    (let ((result (%image-handle-preferred-decoding-colorspace
                   (image-handle-obj handle) colorspace-ptr chroma-ptr)))
      (analyse-error result))
    (values (mem-ref colorspace-ptr 'heif-colorspace)
            (mem-ref chroma-ptr     'heif-chroma))))

(defmacro with-primary-image-handle ((handle context) &body body)
  "Gets the primary image handle from the context and executes body in
scope of that handle. This macro ensures that the handle is released
when the control leaves BODY."
  `(let ((,handle (get-primary-image-handle ,context)))
     (unwind-protect
          (progn ,@body)
       (release-image-handle ,handle))))

(defcfun (%context-image-handle "heif_context_get_image_handle") (:struct heif-error)
  (context :pointer)
  (id      :int)
  (handle  :pointer))

(serapeum:-> context-image-handle (context integer) (values image-handle &optional))
(defun context-image-handle (context id)
  (with-foreign-object (handle-ptr :pointer)
    (let ((result (%context-image-handle (context-obj context) id handle-ptr)))
      (analyse-error result))
    (image-handle (mem-ref handle-ptr :pointer))))

(defmacro with-image-handle ((handle context id) &body body)
  "Get an image handle by id and execute BODY in its scope. Make sure
the handle is released when the control leaves BODY"
  `(let ((,handle (context-image-handle ,context ,id)))
     (unwind-protect (progn ,@body)
       (release-image-handle ,handle))))
