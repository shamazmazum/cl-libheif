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

   ;; Stream reader
   #:stream-reader
   #:with-stream-reader

   ;; Context
   #:context
   #:with-context
   #:context-read-from-file!
   #:context-read-from-octets!
   #:context-read-from-stream!
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
   #:encoder-descriptor
   #:encoder-descriptors
   #:encoder-descriptor-name
   #:encoder-descriptor-id-name
   #:encoder-descriptor-compression-format
   #:encoder-descriptor-supports-lossy-compression-p
   #:encoder-descriptor-supports-lossless-compression-p
   #:encoding-options
   #:+default-encoding-options+
   #:compression-format
   #:with-encoder
   #:with-encoder-for-format
   #:encoder-set-lossy-quality!
   #:encoder-set-lossless!
   #:context-encode-image!

   ;; Images
   #:image
   #:channel
   #:image-height
   #:image-width
   #:image-bits-per-pixel
   #:image-bits-per-pixel-range
   #:image-has-plane-p
   #:image-plane-data
   #:with-image
   #:image-add-plane!
   #:image-set-plane-data!

   ;; Metadata
   #:metadata-ref
   #:metadata-ref-id
   #:metadata-ref-size
   #:image-handle-metadata-refs
   #:image-handle-metadata))
