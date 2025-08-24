(require :asdf)
(let* ((here (or *load-truename* (truename ".")))
			 (dir (make-pathname :name nil :type nil :defaults here))
			 (asd (merge-pathnames "lisp-windows-toolkit.asd" dir)))
	(push dir asdf:*central-registry*)
	(when (probe-file asd)
		(asdf:load-asd asd)))

;; Try to load FiveAM via ASDF if available, but don't require it
(ignore-errors (asdf:load-system :fiveam))

;; Enable coverage and force rebuild of the main system so instrumentation is applied

(format t "[runner] Enabling coverage (SBCL) and loading app ...~%")
(ignore-errors (funcall (read-from-string "lwt.cover:enable-coverage")))
(asdf:load-system :lisp-windows-toolkit :force t)

;; Do not measure tests; disable coverage before loading tests
(ignore-errors (funcall (read-from-string "lwt.cover:disable-coverage")))

;; Load the tests system using ASDF (works with or without Quicklisp)
(format t "[runner] Loading test system :lisp-windows-toolkit/tests ...~%")
(asdf:load-system :lisp-windows-toolkit/tests :force t)

(format t "[runner] Running tests ...~%")
;; Re-enable coverage and run tests so app code execution is recorded
(ignore-errors (funcall (read-from-string "lwt.cover:enable-coverage")))
(let ((res (uiop:symbol-call :lisp-windows-toolkit.tests :run-tests)))
	;; Write the report now that coverage data exists
	;; Write coverage artifact under the project root
	(let* ((base (ignore-errors (asdf:system-source-directory :lisp-windows-toolkit)))
				 (cov-dir (if base (namestring (merge-pathnames "coverage/" base)) "coverage")))
		(ignore-errors (funcall (read-from-string "lwt.cover:write-coverage-report") :directory cov-dir))
		(format t "[runner] Coverage report directory: ~A~%" cov-dir))
  (let* ((status (or (and (find-package :fiveam)
					 (ignore-errors (funcall (read-from-string "fiveam:results-status") res)))
				 :passed))
	    (cov (ignore-errors (funcall (read-from-string "lwt.cover:coverage-percent"))))
	    (fail-tests (and status (not (eql status :passed))))
	    (fail-cov (and (numberp cov) (< cov 100))))
    (when (numberp cov) (format t "Overall coverage: ~D%~%" cov))
    (format t "[runner] Status: ~A~%" status)
    (uiop:quit (if (or fail-tests fail-cov) 1 0))))
