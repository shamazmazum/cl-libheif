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

(defvar *heif-io-stream*)

;; TODO: Make it thread-safe
(defun get-stream ()
  *heif-io-stream*)

(defcallback get-position-callback :int64
    ((userdata :pointer))
  (declare (ignore userdata))
  (let ((stream (get-stream)))
    (file-position stream)))

(defcallback read-callback :int
    ((data     :pointer)
     (size     :size)
     (userdata :pointer))
  (declare (ignore userdata))
  (let ((stream (get-stream))
        (vector (make-array size :element-type '(unsigned-byte 8))))
    (cond
      ((= (read-sequence vector stream) size)
       (loop for i below size do
         (setf (mem-aref data :uint8 i) (aref vector i)))
       0)
      (t 1))))

(defcallback seek-callback :int
    ((position :int64)
     (userdata :pointer))
  (declare (ignore userdata))
  (let ((stream (get-stream)))
    (file-position stream position))
  0)

(defcallback wait-for-size-callback reader-grow-status
    ((target-size :int64)
     (userdata    :pointer))
  (declare (ignore userdata)
           (ignorable target-size))
  ;; Fucking Common Lisp does not have STREAM-FILE-LENGTH! This makes
  ;; this stuff useless with gray streams. Report that the size is
  ;; reached.
  #+nil
  (let* ((stream (get-stream))
         (size (file-length stream)))
    (if (<= target-size size)
        :size-reached
        :size-beyond-eof))
  :size-reached)

(defcstruct heif-reader
  (api-version   :int)
  (get-position  :pointer)
  (read          :pointer)
  (seek          :pointer)
  (wait-for-size :pointer))

(defwrapper stream-reader)

(serapeum:-> make-stream-reader () (values stream-reader &optional))
(defun make-stream-reader ()
  (let ((ptr (foreign-alloc '(:struct heif-reader))))
    (setf (foreign-slot-value ptr '(:struct heif-reader) 'api-version) 1
          (foreign-slot-value ptr '(:struct heif-reader) 'get-position)
          (callback get-position-callback)
          (foreign-slot-value ptr '(:struct heif-reader) 'read)
          (callback read-callback)
          (foreign-slot-value ptr '(:struct heif-reader) 'seek)
          (callback seek-callback)
          (foreign-slot-value ptr '(:struct heif-reader) 'wait-for-size)
          (callback wait-for-size-callback))
    (stream-reader ptr)))

(serapeum:-> free-stream-reader (stream-reader) (values &optional))
(defun free-stream-reader (reader)
  (foreign-free (stream-reader-obj reader))
  (values))

(defmacro with-stream-reader ((reader stream) &body body)
  "Bind @c(reader) to a newly created stream reader which is used in
@c(context-read-from-stream!). All image reading and decoding must be
performed within @c(body) if you read an image from a
stream. @c(Stream) is an actual stream from which the image is
read. It must have the element type @c((unsigned-byte 8))."
  `(let ((,reader (make-stream-reader))
         (*heif-io-stream* ,stream))
     (unwind-protect
          (progn ,@body)
       (free-stream-reader ,reader))))

(defcfun (%read-from-reader! "heif_context_read_from_reader") (:struct heif-error)
  (context  :pointer)
  (reader   (:pointer (:struct heif-reader)))
  (userdata :pointer)
  (options  :pointer))

(serapeum:-> context-read-from-stream! (context stream-reader)
             (values &optional))
(defun context-read-from-stream! (context reader)
  "Read an image from a stream. The reader is created with
@c(with-stream-reader) and all image reading and decoding (not only a
call to this function) must be done in the body of that
macro. Currently, this function also disables multithreaded decoding,
but you can always create as many lisp threads (with their own libheif
contexts) as you wish."
  ;; KLUDGE: Multithreading is hell
  (context-set-max-decoding-threads! context 0)
  (let ((result (%read-from-reader!
                 (context-obj context)
                 (stream-reader-obj reader)
                 (null-pointer) (null-pointer))))
    (analyse-error result))
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
