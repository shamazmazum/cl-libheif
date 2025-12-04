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

(defcfun (%image-plane-readonly "heif_image_get_plane_readonly") :pointer
  (image   :pointer)
  (channel heif-channel)
  (stride  (:pointer :int)))

(serapeum:-> get-plane-data (image channel)
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun get-plane-data (image channel)
  (declare (optimize (speed 3)))
  (with-foreign-object (stride-ptr :int)
    (let ((ptr (%image-plane-readonly (image-obj image) channel stride-ptr)))
      (when (null-pointer-p ptr)
        (error 'no-plane-error))
      (let ((stride (mem-ref stride-ptr :int))
            (plane (make-array (list (image-height image channel)
                                     (image-width  image channel)
                                     (octets-per-pixel
                                      (image-bits-per-pixel image channel)))
                               :element-type '(unsigned-byte 8))))
        (loop for row below (array-dimension plane 0)
              for mem-offset fixnum = (* row stride)
              for plane-offset      = (array-row-major-index plane row 0 0) do
              (loop for idx below (* (array-dimension plane 1)
                                     (array-dimension plane 2))
                    do
                    (setf (row-major-aref plane (+ plane-offset idx))
                          (mem-aref ptr :uint8  (+ mem-offset   idx)))))
        plane))))

;; Image creation

(defcfun (%image-create "heif_image_create") (:struct heif-error)
  (width      :int)
  (height     :int)
  (colorspace heif-colorspace)
  (chroma     heif-chroma)
  (image      :pointer))

(serapeum:-> image-create ((integer 0) (integer 0) colorspace chroma)
             (values image &optional))
(defun image-create (width height colorspace chroma)
  (with-foreign-object (image-ptr :pointer)
    (let ((result (%image-create width height colorspace chroma image-ptr)))
      (analyse-error result))
    (image (mem-ref image-ptr :pointer))))

(defmacro with-image ((image width height colorspace chroma) &body body)
  "Create a new image and execute BODY in its scope. Make sure the
image is released when the control leaves BODY."
  `(let ((,image (image-create ,width ,height ,colorspace ,chroma)))
     (unwind-protect (progn ,@body)
       (image-release ,image))))

(defcfun (%image-add-plane! "heif_image_add_plane") (:struct heif-error)
  (image     :pointer)
  (channel   heif-channel)
  (width     :int)
  (height    :int)
  (bit-depth :int))

(serapeum:-> image-add-plane!
             (image channel (integer 0) (integer 0) (integer 0))
             (values &optional))
(defun image-add-plane! (image channel width height bit-depth)
  (let ((result (%image-add-plane!
                 (image-obj image)
                 channel width height bit-depth)))
    (analyse-error result))
  (values))

(defcfun (%image-plane "heif_image_get_plane") (:pointer :uint8)
  (image   :pointer)
  (channel heif-channel)
  (stride  (:pointer :int)))

(serapeum:-> set-plane-data! (image channel (simple-array (unsigned-byte 8) 3))
             (values &optional))
(defun set-plane-data! (image channel data)
  (declare (optimize (speed 3)))
  (let ((rows   (array-dimension data 0))
        (cols   (array-dimension data 1))
        (octets (array-dimension data 2)))
  (unless (= (octets-per-pixel
              (image-bits-per-pixel image channel))
             octets)
    (error 'bps-error :bps octets))
  (unless (and (= (image-width  image channel) cols)
               (= (image-height image channel) rows))
    (error 'dimensions-mismatch :dims (array-dimensions data)))
  (with-foreign-object (stride-ptr :int)
    (let ((plane (%image-plane (image-obj image) channel stride-ptr)))
      (when (null-pointer-p plane)
        (error 'no-plane-error))
      (let ((stride (mem-ref stride-ptr :int)))
        (loop for r below rows
              for array-offset        = (array-row-major-index data r 0 0)
              for mem-offset fixnum   = (* stride r) do
              (loop for i below (* cols octets) do
                    (setf (mem-aref plane :uint8 (+ mem-offset i))
                          (row-major-aref data (+ array-offset i)))))))))
  (values))
