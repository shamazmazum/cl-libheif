(in-package :cl-libheif)

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

;; Image writing

(defcfun (%context-write-to-file! "heif_context_write_to_file") (:struct heif-error)
  (context  :pointer)
  (filename :string))

(serapeum:-> context-write-to-file! (context (or pathname string)) (values &optional))
(defun context-write-to-file! (context filename)
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
    (let ((*current-output-stream* (tos:make-octet-output-stream)))
      (with-foreign-object (writer-ptr '(:struct heif-writer))
        (with-foreign-slots ((api-version write) writer-ptr (:struct heif-writer))
          (setf api-version 1 write (callback write-to-stream-callback)))
        (let ((result (%context-write (context-obj context) writer-ptr (null-pointer))))
          (analyse-error result)))
      (tos:get-output-stream-octets *current-output-stream*))))
