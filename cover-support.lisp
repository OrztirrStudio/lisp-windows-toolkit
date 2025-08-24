;;; Minimal coverage helper to integrate sb-cover when available (SBCL),
;;; while remaining a no-op on other implementations.

(defpackage :lwt.cover
  (:use :cl)
  (:export :enable-coverage :disable-coverage :write-coverage-report :coverage-percent))

(in-package :lwt.cover)

(defun enable-coverage ()
  #+sbcl
  (progn
    (ignore-errors (require :sb-cover))
    (ignore-errors (funcall (read-from-string "sb-cover:reset")))
    (ignore-errors (funcall (read-from-string "sb-cover:enable-coverage")))
    t)
  #-sbcl
  t)

(defun disable-coverage ()
  #+sbcl
  (progn
    (ignore-errors (funcall (read-from-string "sb-cover:disable-coverage")))
    t)
  #-sbcl
  t)

(defun coverage-percent ()
  "Return 100 when no uncovered forms are detected in sb-cover's report, else 0."
  #+sbcl
  (ignore-errors
    (if (find-package :sb-cover)
        (let ((text (with-output-to-string (s)
                      (funcall (read-from-string "sb-cover:report") :stream s))))
          (if (and text (search "not executed" (string-downcase text))) 0 100))
        nil))
  #-sbcl
  nil)

(defun write-coverage-report (&key (directory "coverage"))
  (ensure-directories-exist directory)
  (let* ((dir (truename directory))
         (path (merge-pathnames (make-pathname :name "coverage" :type "txt") dir)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "Lisp Windows Toolkit - Coverage Report~%")
      (format out "Generated: ~A~%~%" (get-universal-time))
      #+sbcl
      (progn
        (ignore-errors (require :sb-cover))
        (if (find-package :sb-cover)
            (progn
              (format out "[sb-cover] Detailed report below (if available).~%~%")
              (ignore-errors (funcall (read-from-string "sb-cover:report") :stream out)))
            (format out "sb-cover not loaded; coverage details unavailable.~%")))
      #-sbcl
      (format out "Coverage reporting requires SBCL (sb-cover).~%")
      (let ((pct (coverage-percent)))
        (when (numberp pct)
          (format out "~%Overall coverage: ~D%%~%" pct)))))
  t)
