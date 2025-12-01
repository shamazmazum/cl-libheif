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

(defmacro with-primary-image-handle ((handle context) &body body)
  "Gets the primary image handle from the context and executes body in
scope of that handle. This macro ensures that the handle is released
when the control leaves BODY."
  `(let ((,handle (get-primary-image-handle ,context)))
     (unwind-protect
          (progn ,@body)
       (release-image-handle ,handle))))
