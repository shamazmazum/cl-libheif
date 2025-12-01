(in-package :cl-libheif)

;; Error handling
(define-condition cl-libheif-error (error)
  ()
  (:documentation "Generic cl-libheif error"))

(define-condition libheif-error (cl-libheif-error)
  ((message :type    string
            :reader  error-message
            :initarg :message)
   (code    :type    (integer 0)
            :reader  error-code
            :initarg :code)
   (subcode :type    (integer 0)
            :reader  error-subcode
            :initarg :subcode))
  (:report
   (lambda (c s)
     (format s "libheif error: ~a (code ~d, subcode ~d)"
             (error-message c)
             (error-code    c)
             (error-subcode c))))
  (:documentation "libheif error"))

(define-condition bps-error (cl-libheif-error)
  ((bps :type    integer
        :reader  error-bps
        :initarg :bps))
  (:report
   (lambda (c s)
     (format s "Unknown bits per sample: ~d"
             (error-bps c))))
  (:documentation "Signalled by the wrapper when libheif reports unknown BPS value"))

(define-condition no-plane-error (cl-libheif-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "No such plane in the image")))
  (:documentation "Signalled by the wrapper when there is no requested plane in the image"))

(defcstruct (heif-error :class heif-error-type)
  (code    :int)
  (subcode :int)
  (message :string))

(serapeum:defconstructor heif-error
  (code    (integer 0))
  (subcode (integer 0))
  (message string))

(defmethod translate-from-foreign (p (type heif-error-type))
  (let ((plist (call-next-method)))
    (heif-error (getf plist 'code)
                (getf plist 'subcode)
                (getf plist 'message))))

(declaim (inline analyse-error))
(defun analyse-error (error)
  (unless (zerop (heif-error-code error))
    (error 'libheif-error
           :message (heif-error-message error)
           :code    (heif-error-code    error)
           :subcode (heif-error-subcode error))))

;; TODO: add heif_init/heif_deinit

(define-foreign-library libheif
  (:unix (:or "libheif.so.1" "libheif.so"))
  (t (:default "libheif")))
(use-foreign-library libheif)

(defmacro defwrapper (name)
  `(serapeum:defconstructor ,name
     (obj t)))

;; Context creation/destruction
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
  "Execute BODY in libheif context. The context is destroyed when the
control leaves BODY."
  `(let ((,context (alloc-context)))
     (unwind-protect
          (progn ,@body)
       (free-context ,context))))

;; Image reading
(defcfun (%read-from-file! "heif_context_read_from_file") (:struct heif-error)
  (context  :pointer)
  (filename :string)
  (options  :pointer))

(serapeum:-> read-from-file! (context (or string pathname)) (values &optional))
(defun read-from-file! (context filename)
  (let ((result (%read-from-file!
                 (context-obj context)
                 (namestring (truename filename))
                 (null-pointer))))
    (analyse-error result))
  (values))

(defcfun (%read-from-octets! "heif_context_read_from_memory") (:struct heif-error)
  (context  :pointer)
  (memory   :pointer)
  (size     :size)
  (options  :pointer))

(serapeum:-> read-from-octets! (context (simple-array (unsigned-byte 8) (*)))
             (values &optional))
(defun read-from-octets! (context array)
  (with-pointer-to-vector-data (ptr array)
    (let ((result (%read-from-octets!
                   (context-obj context)
                   ptr (length array)
                   (null-pointer))))
      (analyse-error result)))
  (values))

(serapeum:-> read-image! (context (or (simple-array (unsigned-byte 8) (*))
                                      pathname string))
             (values &optional))
(defun read-image! (context source)
  "Read an image from a file or from a simple array of octets"
  (declare (optimize (speed 3)))
  (funcall
   (if (typep source '(or pathname string))
       #'read-from-file! #'read-from-octets!)
   context source))

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
  (%image-handle-width
   (image-handle-obj handle)))


(defcfun (%image-handle-height "heif_image_handle_get_height") :int
  (handle :pointer))

(serapeum:-> image-handle-height (image-handle)
             (values integer &optional)) ; Can it be negative?
(defun image-handle-height (handle)
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

;; Decoding
(defcenum heif-colorspace
  (:undefined 99)
  (:ycbcr     0)
  :rgb
  :monochrome
  :nonvisual)

(deftype colorspace () '(member :undefined :ycbcr :rgb :monochrome :nonvisual))

(defcenum heif-chroma
  (:undefined 99)
  (:monochrome 0)
  :420
  :422
  :444
  (:interleaved-rgb 10)
  :interleaved-rgba
  :interleaved-rrggbb-be
  :interleaved-rrggbbaa-be
  :interleaved-rrggbb-le
  :interleaved-rrggbbaa-le)

(deftype chroma ()
  '(member
    :undefined :monochrome :420 :422 :444
    :interleaved-rgb
    :interleaved-rgba
    :interleaved-rrggbb-be
    :interleaved-rrggbbaa-be
    :interleaved-rrggbb-le
    :interleaved-rrggbbaa-le))

(defwrapper image)
(defwrapper decoding-options)

(defparameter +default-decoding-options+
  (decoding-options (null-pointer)))

(defcfun (%image-release "heif_image_release") :void
  (image :pointer))

(serapeum:-> image-release (image) (values &optional))
(defun image-release (image)
  (%image-release
   (image-obj image))
  (values))

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
  "Decode an image and execute BODY in the scope of that image. Make
sure the image is released when the control leaves BODY."
  `(let ((,image (decode-image ,handle ,colorspace ,chroma ,options)))
     (unwind-protect
          (progn ,@body)
       (image-release ,image))))

;; Data acquisition
(defcenum heif-channel
  :y :cb :cr :r :g :b :alpha
  (:interleaved 10) :filter-array :depth :disparity)
(deftype channel ()
  '(member :y :cb :cr :r :g :b :alpha :interleaved :filter-array :depth :disparity))

(defcfun (%image-width "heif_image_get_width") :int
  (image   :pointer)
  (channel heif-channel))

(serapeum:-> image-width (image channel)
             (values integer &optional)) ; The same question as for image-handle-width
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


#+nil
(cl-libheif:with-context (ctx)
  (cl-libheif:read-image! ctx "~/test/IMG_0007.HEIC")
  (cl-libheif:with-primary-image-handle (handle ctx)
    (cl-libheif:with-decode-image
        (image handle :rgb :interleaved-rgb cl-libheif:+default-decoding-options+)
      (values
       (cl-libheif:image-plane  image :interleaved)
       (cl-libheif:image-width  image :interleaved)
       (cl-libheif:image-height image :interleaved)
       (cl-libheif:image-bits-per-pixel-range image :interleaved)))))

#+nil
(cl-libheif:with-context (ctx)
  (cl-libheif:read-image! ctx "~/test/IMG_0007.HEIC")
  (cl-libheif:with-primary-image-handle (handle ctx)
    (values
     (cl-libheif:image-handle-width  handle)
     (cl-libheif:image-handle-height handle))))
