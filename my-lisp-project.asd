(defsystem "my-lisp-project"
  :description "My first Lisp project."
  :author "Your Name"
  :license "Specify license here"
  :depends-on ()
  :components ((:file "main")))

;; Test system definition (run with: (asdf:test-system :my-lisp-project/tests))
(defsystem "my-lisp-project/tests"
  :description "Test suite for my-lisp-project"
  :depends-on ("my-lisp-project" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam :run! :my-lisp-project)))
