;; PS: run on DrRacket R5Rs

;; eval_utor
; eval_
(define (eval_ exp env)
  (cond ((self-eval_uating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval_-assignment exp env))
        ((definition? exp) (eval_-definition exp env))
        ((if? exp) (eval_-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval_-sequence (begin-actions exp) env))
        ((cond? exp) (eval_ (cond->if exp) env))
        ((application? exp)
         (apply_ (eval_ (operator exp) env)
                (list-of-values (operands exp) env)))
        (else 
          (error "Unknown expression type -- eval_" exp))))

; apply
(define (apply_ procedure arguments) 
  (cond ((primitive-procedure? procedure)
         (apply_-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval_-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
         (else 
           (error "Unknown procedure type -- apply_" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval_ (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval_-if exp env)
  (if (true? (eval_ (if-predicate exp) env))
    (eval_ (if-consequent exp) env)
    (eval_ (if-alternative exp) env)))

(define (eval_-sequence exps env)
  (cond ((last-exp? exps) (eval_ (first-exp exps) env))
        (else (eval_ (first-exp exps) env)
              (eval_-sequence (rest-exps exps) env))))

(define (eval_-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval_ (assignment-value exp) env)
                       env)
  'ok)

(define (eval_-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval_ (definition-value exp) env)
                    env))

; self eval_uating
(define (self-eval_uating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variable
(define (variable? exp) (symbol? exp))

; quote
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; comman helpers
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; assignment
(define (assignment? exp)
  (tagged-list? exp 'set))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; define
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; bool
(define false #f)
(define true #t)
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; env
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; frame
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
;(define the-global-environment (setup-environment))

; primitive procedure
(define apply-in-underlying-scheme apply)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply_-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc)
                              args))

(define input-prompt ";;; M-eval_ input:")
(define output-prompt ";;; M-eval_ value:")

(define (error reason . args)
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg) (display " ") (write arg))
            args)
  (newline)
  (scheme-report-environment -1))  ;; we hope that this will signal an error

(define the-global-environment (setup-environment))
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (display input)
    (let ((output (eval_ input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))


; test

;(driver-loop)
;(define (append x y)
;  (if (null? x)
;    y
;    (cons (car x)
;          (append (cdr x) y))))

;(append '(a b c) '(d e f))

