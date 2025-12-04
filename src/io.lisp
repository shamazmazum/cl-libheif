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

;; Actually, return type is struct heif_error, but I cannot return a
;; struct from a callback. Fortunately, this struct is too small,
;; exactly 16 bytes, so it can be returned in RAX/RDX pair on x86-64.
;; The underlying code will only read the first value, so it's all
;; that matters.
;;
;; This is a very fragile solution, though.
(defcallback write-to-stream-callback :int
    ((context  :pointer)
     (data     :pointer)
     (size     :size)
     (userdata :pointer))
  (declare (ignore context userdata))
  (loop for i below size do
        (write-byte (mem-aref data :uint8 i)
                    *current-output-stream*))
  0)

(defcstruct heif-writer
  (api-version :int)
  (write       :pointer))

(defcfun (%context-write "heif_context_write") (:struct heif-error)
  (context  :pointer)
  (writer   (:pointer (:struct heif-writer)))
  (userdata :pointer))

#+x86-64
(progn
  (serapeum:-> context-write-to-octets (context)
               (values (simple-array (unsigned-byte 8) (*)) &optional))
  (defun context-write-to-octets (context)
    "Write context to a simple array of octets."
    (let ((*current-output-stream* (tos:make-octet-output-stream)))
      (with-foreign-object (writer-ptr '(:struct heif-writer))
        (with-foreign-slots ((api-version write) writer-ptr (:struct heif-writer))
          (setf api-version 1 write (callback write-to-stream-callback)))
        (let ((result (%context-write (context-obj context) writer-ptr (null-pointer))))
          (analyse-error result)))
      (tos:get-output-stream-octets *current-output-stream*))))
