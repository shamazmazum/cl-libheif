(in-package :cl-libheif)

;; Image reading
(defcfun (%context-read-from-file! "heif_context_read_from_file") (:struct heif-error)
  (context  :pointer)
  (filename :string)
  (options  :pointer))

(serapeum:-> context-read-from-file! (context (or string pathname)) (values &optional))
(defun context-read-from-file! (context filename)
  "Read an image from a file."
  (let ((result (%context-read-from-file!
                 (context-obj context)
                 (namestring (truename filename))
                 (null-pointer))))
    (analyse-error result))
  (values))

(defcfun (%context-read-from-octets! "heif_context_read_from_memory") (:struct heif-error)
  (context  :pointer)
  (memory   :pointer)
  (size     :size)
  (options  :pointer))

(serapeum:-> context-read-from-octets! (context (simple-array (unsigned-byte 8) (*)))
             (values &optional))
(defun context-read-from-octets! (context array)
  "Read an image from a simple array of octets (of type @c((unsigned-byte 8)))."
  (with-pointer-to-vector-data (ptr array)
    (let ((result (%context-read-from-octets!
                   (context-obj context)
                   ptr (length array)
                   (null-pointer))))
      (analyse-error result)))
  (values))

;; Image writing

(defcfun (%context-write-to-file! "heif_context_write_to_file") (:struct heif-error)
  (context  :pointer)
  (filename :string))

(serapeum:-> context-write-to-file! (context (or pathname string)) (values &optional))
(defun context-write-to-file! (context filename)
  "Write context to a file."
  (let ((result (%context-write-to-file!
                 (context-obj context)
                 (uiop:native-namestring filename))))
    (analyse-error result))
  (values))

(defvar *current-output-stream*)

(defcallback write-to-stream-callback :void
    ((data     :pointer)
     (size     :size))
  (loop for i below size do
        (write-byte (mem-aref data :uint8 i)
                    *current-output-stream*)))

(defcfun (%context-write "heif_wrapper_context_write") (:struct heif-error)
  (context  :pointer)
  (writer   :pointer))

(serapeum:-> context-write-to-octets (context)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun context-write-to-octets (context)
  "Write context to a simple array of octets."
  (let* ((*current-output-stream* (tos:make-octet-output-stream))
         (result (%context-write (context-obj context)
                                 (callback write-to-stream-callback))))
    (analyse-error result)
    (tos:get-output-stream-octets *current-output-stream*)))
