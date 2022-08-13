(define input-prompt "M-Eval input:")
(define output-prompt "M-Eval value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        ((output (eval input the-global-environment))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline)
    (display string)
    (newline))

(define (announce-output string)
    (newline)
    (display string)
    (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list
                    'compound-procedure
                    (procedure-parameters objects)
                    (procedure-body objects)
                    "<procedure-env>"))
        (display object)))