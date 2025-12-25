(defpackage cl-libheif/tests
  (:use #:cl #:cl-libheif #:fiveam)
  (:local-nicknames (#:ff #:float-features)
                    (#:fs #:flexi-streams))
  (:export #:run-tests))
