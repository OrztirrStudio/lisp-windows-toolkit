(defsystem "lisp-windows-toolkit"
  :description "One-click Common Lisp development environment for Windows"
  :author "Nicholas Cole Akers"
  :license "MIT"
  :depends-on ()
  :components ((:file "main")
               (:file "cover-support")))

;; Test system definition (run with: (asdf:test-system :lisp-windows-toolkit/tests))
(defsystem "lisp-windows-toolkit/tests"
  :description "Test suite for lisp-windows-toolkit"
  :depends-on ("lisp-windows-toolkit" "fiveam")
  :components ((:file "cover-support")
               (:file "tests"))
  :perform (test-op (o c)
             ;; Run our test entry point; coverage is handled by the external runner
             (uiop:symbol-call :lisp-windows-toolkit.tests :run-tests)))