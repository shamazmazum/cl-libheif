(in-package :cl-libheif)

;; Encoder options

(defwrapper encoding-options)
(defparameter +default-encoding-options+
  (encoding-options (null-pointer))
  "Default encoding options")

;; Decoder options

(defwrapper decoding-options)
(defparameter +default-decoding-options+
  (decoding-options (null-pointer))
  "Default decoding options")

(defcstruct heif-decoding-options
  (version                :uint8)
  ;; V1
  ;; FIXME: bool
  (ignore-transformations :uint8)
  (start-progress         :pointer)
  (on-progress            :pointer)
  (end-progress           :pointer)
  (progress-user-data     :pointer)
  ;; V2
  ;; FIXME: bool
  (convert-hdr-to-8bit    :uint8)
  ;; V3
  ;; Ditto
  (strict-decoding        :uint8)
  ;; V4
  ;; String, actually
  (decoder-id             :pointer)
  ;; V5
  ;; FIXME: enum
  (color-conversion-opts  :uint32)
  ;; V6
  (cancel-decoding        :pointer))
