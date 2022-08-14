;;;;;;;;;;;;;;;;;;;;;;;;;;;
;一些宏和重命名
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (text-of-quotation exp) (cadr exp))
(define apply-in-underlying-scheme apply)
(define (make-procedure parameters body env)
    (list 'procedure parameters body env))
;变量赋值
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;变量定义
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))
;过程组成
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;if
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
    (if (null? (cdddr exp))
        #f
        (cadddr exp)))
(define (make-if predicate consequent alternative) (list 'if predicate consequent alternative))
;begin
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (begin-action exp) (cdr exp))
;cond
(define (cond-clause exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cadr clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
;过程
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (first-operands exps) (car exps))
(define (rest-operands exps) (cdr exps))
(define (no-operands? exps) (null? exps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;环境相关操作
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! variable value frame)
    (set-car! frame (cons variable (frame-variables frame)))
    (set-cdr! frame (cons value (frame-values frame))))

(define (extend-environment variables values base-env)
    (if (= (length variables) (length values))
        (cons (make-frame variables values) base-env)
        (begin (display "extend environment error: wrong argument count") (newline))))
;查找变量
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan variables values)
            (cond
                ((null? variables)
                    (env-loop (enclosing-environment env)))
                ((eq? var (car variables))
                    (car values))
                (else (scan (cdr variables) (cdr values)))))
        (if (eq? env the-empty-environment)
            (begin (display "LOOK UP: Unbound variable ") (display var) (newline))
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))
;定义变量
(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan variables values)
            (cond
                ((null? variables)
                    (add-binding-to-frame! var val frame))
                ((eq? var (car variables))
                    (set-car! variables val))
                (else
                    (scan (cdr variables) (cdr values)))))
        (scan (frame-variables frame) (frame-values frame))))
;设置变量
(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan variables values)
            (cond
                ((null? variables)
                    (env-loop (enclosing-environment env)))
                ((eq? var (car variables))
                    (set-car! values val))
                (else (scan (cdr variables) (cdr values)))))
        (if (eq? env the-empty-environment)
            (begin (display "SET: Unbound variable ") (display var) (newline))
            (let ((frame (first-frame env)))
                (scan (frame-variables frame) (frame-values frame)))))
    (env-loop env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        #f))
;判断
(define (self-evaluating? exp)
    (cond   ((number? exp) #t)
            ((string? exp) #t)
            ((boolean? exp) #t)
            ((tagged-list? exp 'procedure) #t)
            (else #f)))
(define (variable? exp)
    (symbol? exp))
(define (quoted? exp)(tagged-list? exp 'quote))
(define (assignment? exp)(tagged-list? exp 'set))
(define (definition? exp) (tagged-list? exp 'define))
(define (lambda? exp)(tagged-list? exp 'lambda))
(define (if? exp)(tagged-list? exp 'if))
(define (begin? exp) (tagged-list? exp 'begin))
(define (cond? exp) (tagged-list? exp 'cond))
(define (application? exp) (pair? exp))
;赋值
(define (eval-assignment exp env)
    (set-variable-value!
        (assignment-variable exp)
        (eval (assignment-value exp) env)
        env))
;定义
(define (eval-definition exp env)
    (define-variable!
        (definition-variable exp)
        (eval (definition-value exp) env)
        env))
;if
(define (eval-if exp env)
    (if (eval (if-predicate exp) env)
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
;begin
(define (eval-sequence exps env)
    (cond
        ((last-exp? exps)
            (eval (first-exp exps) env))
        (else
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))
;cond
(define (cond->if exp) (expand-clauses (cond-clause exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        #f
        (let ((first (car clauses)) (rest (cdr clauses)))
            (if (cond-else-clause? first)
                ((if
                    (null? rest)
                    (cond-actions first)
                    (display "ELSE clause isn't last\n")))
                (make-if
                    (cond-predicate first)
                    (cond-actions first)
                    (expand-clauses rest))))))
;组合式
(define (list-of-value exps env)
    (if (no-operands? exps)
        '()
        (cons   (eval (first-operands exps) env)
                (list-of-value (rest-operands exps) env))))
;eval
(define (eval exp env)
    (cond
            ;基本表达式
            ((self-evaluating? exp) exp) ;ok
            ((variable? exp) (lookup-variable-value exp env)) ;ok
            ;特殊形式
            ((quoted? exp) (text-of-quotation exp)) ;ok
            ((assignment? exp) (eval-assignment exp env)) ;ok
            ((definition? exp) (eval-definition exp env)) ;ok
            ((if? exp) (eval-if exp env)) ;ok
            ((lambda? exp)
                (make-procedure (lambda-parameters exp)
                                (lambda-body exp)
                                env))
            ((begin? exp) (eval-sequence (begin-action exp) env)) ;ok
            ((cond? exp) (eval (cond->if exp) env)) ;ok
            ;组合式
            ((application? exp)
                (apply  (eval (operator exp) env)
                        (list-of-value (operands exp) env)))
            ;其他
            (else (display "Unknown expression type\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (compound-procedure? p) (tagged-list? p 'procedure))

(define (apply procedure arguments)
    (cond
        ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)) ;ok
        ((compound-procedure? procedure)
            (eval-sequence
                (procedure-body procedure)
                (extend-environment
                    (procedure-parameters procedure)
                    arguments
                    (procedure-environment procedure))))
        (else (display "Unknown procedure type\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;初始环境设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
    (list
        (list 'and (lambda (x y) (and x y))) (list 'or (lambda (x y) (or x y))) (list 'not not)
        (list 'apply apply) (list 'eval eval)
        (list 'cons cons)
        (list 'eq (lambda (x y) (eq? x y))) (list 'equal equal?)
        (list 'error error)
        (list 'car car) (list 'cdr cdr) (list 'cadr cadr) (list 'cdar cdar)
        (list 'cadar cadr) (list 'caddr cadr) (list 'cdaar cdar) (list 'cdadr cdar)
        (list 'cadaar cadr) (list 'cadadr cadr) (list 'caddar cadr) (list 'cadddr cadr)
        (list 'cdaaar cdar) (list 'cdaadr cdar) (list 'cdadar cdar) (list 'cdaddr cdar)
        (list 'print print)
        (list 'plus +) (list 'minus -)
        (list 'exit exit)
        ;;;
    ))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects) (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (setup-environment)
    (let ((initial-env
            (extend-environment
                (primitive-procedure-names)
                (primitive-procedure-objects)
                the-empty-environment)))
        (define-variable! 'true #t initial-env)
        (define-variable! 'false #f initial-env)
        initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-prompt "M-Eval input: ")
(define output-prompt "M-Eval value: ")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop))

(define (prompt-for-input string)
    (newline)
    (display string))

(define (announce-output string)
    (display string))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list
                    'compound-procedure
                    (procedure-parameters object)
                    (procedure-body object)
                    "<procedure-env>"))
        (display object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(driver-loop)