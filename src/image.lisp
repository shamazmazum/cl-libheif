(in-package :cl-libheif)

(defwrapper image)

(defcfun (%image-release "heif_image_release") :void
  (image :pointer))

(serapeum:-> image-release (image) (values &optional))
(defun image-release (image)
  (%image-release
   (image-obj image))
  (values))

(defcfun (%image-width "heif_image_get_width") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-width (image channel)
             (values integer &optional)) ; Can it be negative somehow?
(defun image-width (image channel)
  (%image-width (image-obj image) channel))

(defcfun (%image-height "heif_image_get_height") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-height (image channel)
             (values integer &optional)) ; Ditto
(defun image-height (image channel)
  (%image-height (image-obj image) channel))


(defcfun (%image-bits-per-pixel "heif_image_get_bits_per_pixel") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-bits-per-pixel (image channel)
             (values integer &optional)) ; Ditto
(defun image-bits-per-pixel (image channel)
  (%image-bits-per-pixel (image-obj image) channel))

(defcfun (%image-bits-per-pixel-range "heif_image_get_bits_per_pixel_range") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-bits-per-pixel-range (image channel)
             (values integer &optional)) ; Ditto
(defun image-bits-per-pixel-range (image channel)
  (%image-bits-per-pixel-range (image-obj image) channel))



;; Data acquisition
(serapeum:-> octets-per-pixel (integer)
             (values (member 1 3 4) &optional))
(defun octets-per-pixel (bps)
  (cond
    ((= bps 8)  1) ; Monochrome
    ((= bps 24) 3) ; RGB
    ((= bps 32) 4) ; RGBA
    (t (error 'bps-error :bps bps))))

(defcfun (%image-plane "heif_image_get_plane_readonly") :pointer
  (image   :pointer)
  (channel heif-channel)
  (stride  (:pointer :int)))

(serapeum:-> image-plane (image channel)
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun image-plane (image channel)
  (declare (optimize (speed 3)))
  (with-foreign-object (stride-ptr :int)
    (let ((ptr (%image-plane (image-obj image) channel stride-ptr)))
      (when (null-pointer-p ptr)
        (error 'no-plane-error))
      (let ((stride (mem-ref stride-ptr :int))
            (plane (make-array (list (image-height image channel)
                                     (image-width  image channel)
                                     (octets-per-pixel
                                      (image-bits-per-pixel image channel)))
                               :element-type '(unsigned-byte 8))))
        (loop for row below (array-dimension plane 0)
              for mem-offset   = (* row stride)
              for plane-offset = (array-row-major-index plane row 0 0) do
              (loop for idx below (* (array-dimension plane 1)
                                     (array-dimension plane 2))
                    do
                    (setf (row-major-aref plane (+ plane-offset idx))
                          (mem-aref ptr :uint8  (+ mem-offset   idx)))))
        plane))))
