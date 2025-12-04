(defpackage cl-libheif
  (:use #:cl #:cffi)
  (:local-nicknames (#:tos #:trivial-octet-streams))
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
   #:context-write-to-octets
   #:context-top-level-image-ids
   #:context-primary-image-id
   #:context-set-maximum-image-size-limit!
   #:context-set-max-decoding-threads!

   ;; Image handle
   #:image-handle
   #:with-primary-image-handle
   #:with-image-handle
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
   #:set-plane-data!

   ;; Metadata
   #:metadata-ref
   #:metadata-ref-id
   #:metadata-ref-size
   #:get-metadata-refs
   #:get-metadata))
