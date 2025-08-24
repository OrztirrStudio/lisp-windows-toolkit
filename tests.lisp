(defpackage :lisp-windows-toolkit.tests
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :lisp-windows-toolkit.tests)

(def-suite :lisp-windows-toolkit)
(in-suite :lisp-windows-toolkit)

(test hello-prints
  (is (string= (with-output-to-string (s)
                 (let ((*standard-output* s))
                   (lisp-windows-toolkit:main)))
               (concatenate 'string "Hello from Lisp Windows Toolkit!" (string #\Newline)))))

(defun run-tests ()
  (let ((results (run! :lisp-windows-toolkit)))
    (ignore-errors (funcall (read-from-string "fiveam:explain") results))
    results))

;; Coverage is controlled by the runner now.
