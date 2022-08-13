;重命名
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        #f))

(define (text-of-quotation exp) (cdr exp))

(define (assignment-varialbe exp) (car exp))
(define (assignmen-value exp) (cdr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (definition-varialbe exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))

(define (lambda-parameters exp) (car exp))
(define (lambda-body) (cdr exp))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
    (if (null? (cdddr exp))
        #f
        (cadddr exp)))

(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (begin-action exp) (cdr exp))

(define (cond-clause exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (first-operands exps) (car exps))
(define (rest-operands exps) (cdr exps))
(define (no-operands? exps) (null? exps))