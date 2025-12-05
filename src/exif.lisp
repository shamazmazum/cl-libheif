(in-package :cl-libheif)

(defcfun (number-of-metadata-blocks "heif_image_handle_get_number_of_metadata_blocks") :int
  (handle :pointer)
  (filter :string))

(defcfun (metadata-block-ids "heif_image_handle_get_list_of_metadata_block_IDs") :int
  (handle :pointer)
  (filter :string)
  (ids    (:pointer :uint32))
  (count  :int))

(defcfun (metadata-block-get-size "heif_image_handle_get_metadata_size") :size
  (handle :pointer)
  (id     :uint32))

(defcfun (metadata-block-get-data "heif_image_handle_get_metadata") (:struct heif-error)
  (handle :pointer)
  (id     :uint32)
  (data   :pointer))

(serapeum:defconstructor metadata-ref
  (size (unsigned-byte 64))
  (id   (unsigned-byte 32)))

(serapeum:-> image-handle-metadata-refs (image-handle &optional (or string null) (integer 0))
             (values list &optional))
(defun image-handle-metadata-refs (handle &optional filter (max-ids 0))
  "Get a list of metadata blocks. If MAX-IDS is positive, return no
more than MAX-IDS entries."
  (let* ((filter (if filter filter (null-pointer)))
         (number (number-of-metadata-blocks
                  (image-handle-obj handle) filter))
         (number (if (> max-ids 0) (min number max-ids) number)))
    (with-foreign-object (ids-ptr :uint32 number)
      (let ((received-number (metadata-block-ids
                              (image-handle-obj handle)
                              filter ids-ptr number)))
        (assert (= number received-number)))
      (loop for i below number
            for id = (mem-aref ids-ptr :uint32 i)
            collect
            (metadata-ref
             (metadata-block-get-size
              (image-handle-obj handle) id)
             id)))))

(serapeum:-> image-handle-metadata (image-handle metadata-ref)
             (values (simple-array (unsigned-byte 8) (*)) &optional))
(defun image-handle-metadata (handle ref)
  "Get a metadata block by its reference."
  (let* ((size (metadata-ref-size ref))
         (result (make-shareable-byte-vector size)))
    (with-pointer-to-vector-data (data-ptr result)
      (let ((result (metadata-block-get-data
                     (image-handle-obj handle)
                     (metadata-ref-id ref)
                     data-ptr)))
        (analyse-error result)))
    result))
