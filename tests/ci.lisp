(defun do-all()
  (ql:quickload :cl-libheif/tests)
  (uiop:quit
   (if (uiop:call-function "cl-libheif/tests:run-tests")
       0 1)))

(do-all)
