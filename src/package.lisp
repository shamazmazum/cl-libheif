(defpackage cl-libheif
  (:use #:cl #:cffi)
  (:export
   ;; Conditions
   #:cl-libheif-error
   #:libheif-error
   #:bps-error
   #:no-plane-error
   #:dimensions-mismatch

   ;; libheif (de)initialization
   #:init-parameters
   #:+default-init-parameters+
   #:with-libheif

   ;; Context
   #:context
   #:with-context
   #:read-image!
   #:context-write-to-file!

   ;; Image handle
   #:image-handle
   #:with-primary-image-handle
   #:image-handle-width
   #:image-handle-height
   #:image-handle-preferred-decoding-colorspace

   ;; Colorspaces
   #:colorspace
   #:chroma

   ;; Decoder
   #:decoding-options
   #:+default-decoding-options+
   #:with-decode-image

   ;; Encoder
   #:encoding-options
   #:+default-encoding-options+
   #:compression-format
   #:with-encoder-for-format
   #:encoder-set-lossy-quality!
   #:context-encode-image

   ;; Images
   #:image
   #:channel
   #:image-height
   #:image-width
   #:image-bits-per-pixel
   #:image-bits-per-pixel-range
   #:get-plane-data
   #:with-image
   #:image-add-plane!
   #:set-plane-data!))
