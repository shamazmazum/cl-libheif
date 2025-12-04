(in-package :cl-libheif/tests)

(def-suite libheif :description "Test cl-libheif wrapper")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(libheif))))

(serapeum:-> interleave-planes
             ((simple-array (unsigned-byte 8) 2)
              (simple-array (unsigned-byte 8) 2)
              (simple-array (unsigned-byte 8) 2))
             (values (simple-array (unsigned-byte 8) (* * 3)) &optional))
(defun interleave-planes (r g b)
  (assert (and (= (array-dimension r 0)
                  (array-dimension g 0)
                  (array-dimension b 0))
               (= (array-dimension r 1)
                  (array-dimension g 1)
                  (array-dimension b 1))))
  (let ((result (make-array (list (array-dimension r 0)
                                  (array-dimension r 1)
                                  3)
                            :element-type '(unsigned-byte 8))))
    (loop for i below (array-dimension result 0) do
          (loop for j below (array-dimension result 1) do
                (setf (aref result i j 0) (aref r i j)
                      (aref result i j 1) (aref g i j)
                      (aref result i j 2) (aref b i j))))
    result))

(serapeum:-> create-noise ((integer 0) (integer 0) (integer 0) &optional (single-float 0.0))
             (values (simple-array (unsigned-byte 8) 2) &optional))
(defun create-noise (height width seed &optional (mul 10.0))
  (let ((result (make-array (list height width) :element-type '(unsigned-byte 8))))
    (loop for i below height do
          (loop for j below width do
                (setf (aref result i j)
                      (floor
                       (* (cl-value-noise:value-noise
                           (* (/ i height) mul)
                           (* (/ j width)  mul)
                           0.0 :seed seed)
                          255)))))
    result))

(serapeum:-> encode-image
             ((integer 0) (integer 0) &optional (single-float 0.0))
             (values (simple-array (unsigned-byte 8) (*))
                     (simple-array (unsigned-byte 8) (* * 3))
                     &optional))
(defun encode-image (height width &optional (mul 10.0))
  (let* ((r (create-noise height width (random 100000) mul))
         (g (create-noise height width (random 100000) mul))
         (b (create-noise height width (random 100000) mul))
         (data (interleave-planes r g b)))
    (with-libheif (+default-init-parameters+)
      (with-image (image width height :rgb :interleaved-rgb)
        (image-add-plane! image :interleaved width height 24)
        (set-plane-data!  image :interleaved data)
        (with-context (context)
          (with-encoder-for-format (encoder context :hevc)
            (encoder-set-lossy-quality! encoder 95)
            (context-encode-image!
             context image encoder
             +default-encoding-options+))
          (values (context-write-to-octets context) data))))))

(serapeum:-> decode-image
             ((simple-array (unsigned-byte 8) (*)))
             (values (simple-array (unsigned-byte 8) (* * 3)) &optional))
(defun decode-image (data)
  (with-libheif (+default-init-parameters+)
    (with-context (ctx)
      (context-read-from-octets! ctx data)
      (with-primary-image-handle (handle ctx)
        (with-decode-image (image handle :rgb :interleaved-rgb +default-decoding-options+)
          (get-plane-data image :interleaved))))))

(serapeum:-> image-diff ((simple-array (unsigned-byte 8) (* * 3))
                         (simple-array (unsigned-byte 8) (* * 3)))
             (values unsigned-byte &optional))
(defun image-diff (image1 image2)
  (assert (equalp (array-dimensions image1)
                  (array-dimensions image2)))
  (loop for i below (array-total-size image1) sum
        (abs (- (row-major-aref image1 i)
                (row-major-aref image2 i)))))

(in-suite libheif)

(test check-encoder/decoder-interleaved
  (ff:with-float-traps-masked (:overflow :divide-by-zero :invalid)
    (loop repeat 100
          for width  = (+ 100 (random 1000))
          for height = (+ 100 (random 1000)) do
          (multiple-value-bind (data image-expected)
              (encode-image height width)
            (let ((image-received (decode-image data)))
              (is (< (image-diff image-expected image-received)
                     (* 128 3 height width 5f-2))))))))

(test check-image-dimensions
  (ff:with-float-traps-masked (:overflow :divide-by-zero :invalid)
    (loop repeat 100
          for width  = (+ 100 (random 1000))
          for height = (+ 100 (random 1000))
          for data   = (encode-image height width) do
          (with-libheif (+default-init-parameters+)
            (with-context (ctx)
              (context-read-from-octets! ctx data)
              (with-primary-image-handle (handle ctx)
                (is (= width  (image-handle-width handle)))
                (is (= height (image-handle-height handle)))))))))
