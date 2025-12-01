(defpackage cl-libheif
  (:use #:cl #:cffi)
  (:export #:cl-libheif-error
           #:libheif-error
           #:bps-error
           #:no-plane-error

           #:context
           #:with-context
           #:read-image!
           #:image-handle
           #:with-primary-image-handle
           #:image-handle-width
           #:image-handle-height
           #:colorspace
           #:chroma
           #:decoding-options
           #:+default-decoding-options+
           #:image
           #:with-decode-image
           #:channel
           #:image-height
           #:image-width
           #:image-bits-per-pixel
           #:image-bits-per-pixel-range
           #:image-plane))
