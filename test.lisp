;; Load ASDF first
(require :asdf)

;; Add current directory to ASDF's search path
(push #P"./" asdf:*central-registry*)

;; Now load our system
(asdf:load-system :lisp-windows-toolkit)

;; Call our main function
(lisp-windows-toolkit:main)
