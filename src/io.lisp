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
;; TODO: implement writing to sequence

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
