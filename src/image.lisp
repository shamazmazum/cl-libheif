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
  "Get width of a channel in the image."
  (%image-width (image-obj image) channel))

(defcfun (%image-height "heif_image_get_height") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-height (image channel)
             (values integer &optional)) ; Ditto
(defun image-height (image channel)
  "Get height of a channel in the image."
  (%image-height (image-obj image) channel))


(defcfun (%image-bits-per-pixel "heif_image_get_bits_per_pixel") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-bits-per-pixel (image channel)
             (values integer &optional))
(defun image-bits-per-pixel (image channel)
  "Get the number of bits per pixel in the given image
channel. Returns -1 if a non-existing channel was given. Note that the
number of bits per pixel may be different for each color channel.
This function returns the number of bits used for storage of each
pixel. Especially for HDR images, this is probably not what you
want. Have a look at @c(image-bits-per-pixel-range)."
  (%image-bits-per-pixel (image-obj image) channel))

(defcfun (%image-bits-per-pixel-range "heif_image_get_bits_per_pixel_range") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-bits-per-pixel-range (image channel)
             (values integer &optional))
(defun image-bits-per-pixel-range (image channel)
  "Get the number of bits per pixel in the given image channel. This
function returns the number of bits used for representing the pixel
value, which might be smaller than the number of bits used in memory.

For example, in 12bit HDR images, this function returns '12', while
still 16 bits are reserved for storage. For interleaved RGBA with 12
bit, this function also returns '12', not '48' or '64'
(@c(image-bits-per-pixel) returns 64 in this case)."
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

(serapeum:-> image-plane-data (image channel)
             (values (simple-array (unsigned-byte 8) 3) &optional))
(defun image-plane-data (image channel)
  "Get a simple array of octets which contains plane data. In the case
of monochrome channels (@c(:monochrome), @c(:r), @c(:y) etc.) the
returned array has dimensions @c((height width 1)). In the case of
interleaved color channels the returned array has dimensions @c((height
width 3)) or @c((height weight 4)) (the latter with alpha channel)."
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

(serapeum:-> image-has-plane-p (image channel)
             (values boolean &optional))
(defun image-has-plane-p (image channel)
  "Return @c(t) if an image has a plane specified by @c(channel), @c(nil) otherwise."
  (let ((ptr (%image-plane-readonly (image-obj image) channel (null-pointer))))
    (not (null-pointer-p ptr))))

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
  "Bind @c(image) to a new image and execute @c(body) in its
scope. Make sure the image is released when the control leaves
@c(body)."
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
  "Add a plane to an image. The actual data must be transferred to the
plane via @c(image-set-plane-data!)."
  (let ((result (%image-add-plane!
                 (image-obj image)
                 channel width height bit-depth)))
    (analyse-error result))
  (values))

(defcfun (%image-plane "heif_image_get_plane") (:pointer :uint8)
  (image   :pointer)
  (channel heif-channel)
  (stride  (:pointer :int)))

(serapeum:-> image-set-plane-data! (image channel (simple-array (unsigned-byte 8) 3))
             (values &optional))
(defun image-set-plane-data! (image channel data)
  "Copy a simple array of octets to a plane which is previously
created with @c(image-add-plane!). The array must have dimensions
@c((height width 1)) for monochrome/one color per channel data or
@c((height width 3)) / @c((height width 4)) for interleaved color data
(the latter with alpha)."
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
