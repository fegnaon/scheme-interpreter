(define (setup-environment)
    (let ((initial-env
            (extend-environment
                (primitive-procedure-names)
                (primitive-procedure-objects)
                the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))



(define (primitive-procedure? proc) (tagged-list? proc 'procedure))

(define primitive-procedures
    (list
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        ;;;
    ))

(define (primitive-implementation proc) (cadr proc))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))x`

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment) (cadddr p))

(define (apply procedure arguments)
    (cond(
        ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
            (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-parameters procedure)
                    arguments
                    (procedure-environment procedure))))

        (else (error "Unknown procedure type\n")))))