;; Print a clear banner so the user knows input is active
(format t "~%[REPL] Ready. Try: (+ 1 2) => 3~%")
;; Ensure a visible prompt on SBCL; safe if not present
(ignore-errors
  (setf (symbol-value (intern "*REPL-PROMPT-FUNCTION*" :sb-impl))
        (lambda (stream) (format stream "CL-USER> "))))