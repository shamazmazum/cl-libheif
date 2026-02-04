(defun do-all()
  (handler-case
      (asdf:load-system :cl-libheif/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "cl-libheif/tests:run-tests")
       0 1)))

(do-all)
