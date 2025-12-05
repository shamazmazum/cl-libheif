(in-package :cl-libheif)

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
  (:documentation "Libheif error. It's signalled when libheif returns
an error code."))

(define-condition bps-error (cl-libheif-error)
  ((bps :type    integer
        :reader  error-bps
        :initarg :bps))
  (:report
   (lambda (c s)
     (format s "Unknown bits per sample: ~d"
             (error-bps c))))
  (:documentation "Signalled by the wrapper when libheif reports
a BPS value which is unsupported by this wrapper."))

(define-condition no-plane-error (cl-libheif-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "No such plane in the image")))
  (:documentation "Signalled by the wrapper when there is no requested
plane in the image."))

(define-condition dimensions-mismatch (cl-libheif-error)
  ((dims :initarg :dims
         :reader  mismatched-dims))
  (:report
   (lambda (c s)
     (format s "No plane with dimensionality ~a"
             (mismatched-dims c))))
  (:documentation "Signalled when trying to set data of the wrong
dimensionality to a plane."))

;; Internal error struct
(defcstruct (heif-error :class heif-error-type)
  (code    :int)
  (subcode :int)
  (message :string))

;; Lisp-side error struct
(serapeum:defconstructor heif-error
  (code    (integer 0))
  (subcode (integer 0))
  (message string))

(defmethod translate-from-foreign (p (type heif-error-type))
  (let ((plist (call-next-method)))
    (heif-error (getf plist 'code)
                (getf plist 'subcode)
                (getf plist 'message))))

;; Signal LIBHEIF-ERROR if ERROR has an error code
(declaim (inline analyse-error))
(defun analyse-error (error)
  (unless (zerop (heif-error-code error))
    (error 'libheif-error
           :message (heif-error-message error)
           :code    (heif-error-code    error)
           :subcode (heif-error-subcode error))))
