;自求职表达式
(define (self-evaluating? exp)
    (cond   ((number? exp) #t)
            ((string? exp) #t)
            (else #f)))
;变量
(define (variable? exp) (symbol? exp))
;加引号的表达式
(define (quoted? exp)(tagged-list? exp 'quote))
;赋值
(define (assignment? exp)(tagged-list? exp 'set!))
(define (eval-assignment exp env)
    (set-variable-value!
        (assignment-variable exp)
        (eval (assignment-value exp) env)
        env))
;定义
(define (definition? exp) (tagged-list? exp 'define))
(define (eval-definition exp)
    (definition-variable!
        (definition-variable exp)
        (eval (definition-value exp) env)
        env))
;lambda
(define (lambda? exp)(tagged-list? exp 'lambda))
;if
(define (if? exp)(tagged-list? exp 'if))
(define (eval-if exp)
    (if (eval (if-predicate exp) env)
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-action exp) (cdr exp))
(define (eval-sequence exps env)
    (cond
        ((last-exp? exps)
            (eval (first-exp exps) env)
        (else
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env)))))
;cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clause exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))

(define (cond->if exp) (expand-clauses (cond-clause exp)))

(define (expand-clauses clauses)
    (if (null? clauses)))
        #f
        (let ((first (car clauses)) (rest (cdr clauses)))
            (if (cond-else-clause? first)
                ((if (null? rest))
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last"))
                (make-if
                    (cond-predicate first)
                    (cond-actions first)
                    (expand-clauses rest))))

;组合式
(define (application? exp) (pair? exp))
(define (list-of-value exps env)
    (if (no-operands? exps)
        ()
        (cons   (eval (first-operands exps) env)
                (list-of-values (rest-operands exps) env))))


;eval
(define (eval exp env)
    ((cond  ;基本表达式
            ((self-evaluating? exp) exp)
            ((variable? exp) (lookup-variable-value exp env))
            ;特殊形式
            ((quoted? exp)(text-of-quotation exp))
            ((assignment? exp)(eval-assignment exp env))
            ((definition? exp)(eval-definition exp env))
            ((if? exp)(eval-if exp env))
            ((lambda? exp)
                (make-procedure (lambda-parameters exp)
                                (lambda-body exp)
                                env))
            ((begin? exp)(eval-sequence (begin-action exp) env))
            ((cond? exp) (eval (cond->if exp) env))
            ;组合式
            ((application? exp)
                (apply  (eval (operator exp) env)
                        (list-of-value (operands exp) env)))
            ;其他
            (else (error "Unknown expression type\n")))))