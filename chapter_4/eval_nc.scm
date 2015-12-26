;; PS: run on DrRacket R5Rs

;; evalutor
; ambeval
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

; analyze
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((amb? exp) (analyze-amb exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

; apply
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply_-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (ambeval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (ambeval-if exp env)
  (if (true? (ambeval (if-predicate exp) env))
    (ambeval (if-consequent exp) env)
    (ambeval (if-alternative exp) env)))

(define (ambeval-sequence exps env)
  (cond ((last-exp? exps) (ambeval (first-exp exps) env))
        (else (ambeval (first-exp exps) env)
              (ambeval-sequence (rest-exps exps) env))))

(define (ambeval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (ambeval (assignment-value exp) env)
                       env)
  'ok)

(define (ambeval-definition exp env)
  (define-variable! (definition-variable exp)
                    (ambeval (definition-value exp) env)
                    env))

; self evaluating
(define (self-evaluating? exp)
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
  (tagged-list? exp 'set!))
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

; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let->combination exp) 
  (let ((block (make-lambda (map car (let-bindings exp)) (let-body exp))))
    (cons block (map cadr (let-bindings exp)))))

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

;; amb
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))


(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

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
        (list 'member member)
        (list 'memq memq)
        (list 'not not)
        (list 'list list)
        (list 'abs abs)
        (list '> >)
        (list '< <)
        (list '= =)
        (list '- -)
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

(define (error reason . args)
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg) (display " ") (write arg))
            args)
  (newline)
  (scheme-report-environment -1))  ;; we hope that this will signal an error

(define the-global-environment (setup-environment))
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

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


;; Logic puzzles
;(define (require p)
;  (if (not p) (amb)))

;(define (an-element-of items)
;  (require (not (null? items)))
;  (amb (car items) (an-element-of (cdr items))))
;
;(define (distinct? items)
;  (cond ((null? items) true)
;        ((null? (cdr items)) true)
;        ((member (car items) (cdr items)) false)
;        (else (distinct? (cdr items)))))
;
;(define (multiple-dwelling)
;  (let ((baker (amb 1 2 3 4 5))
;        (cooper (amb 1 2 3 4 5))
;        (fletcher (amb 1 2 3 4 5))
;        (miller (amb 1 2 3 4 5))
;        (smith (amb 1 2 3 4 5)))
;    (require
;     (distinct? (list baker cooper fletcher miller smith)))
;    (require (not (= baker 5)))
;    (require (not (= cooper 1)))
;    (require (not (= fletcher 5)))
;    (require (not (= fletcher 1)))
;    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
;    (require (not (= (abs (- fletcher cooper)) 1)))
;    (list (list 'baker baker)
;          (list 'cooper cooper)
;          (list 'fletcher fletcher)
;          (list 'miller miller)
;          (list 'smith smith))))
; (multiple-dwelling)


;; Parsing nature language
;(define nouns '(noun student professor cat class))
;(define verbs '(verb studies lectures eats sleeps))
;(define articles '(article the a))
;
;;(define (parse-sentence)
;;  (list 'sentence
;;         (parse-noun-phrase)
;;         (parse-word verbs)))
;
;(define (parse-noun-phrase)
;  (list 'noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))
;
;(define (parse-word word-list)
;  (require (not (null? *unparsed*)))
;  (require (memq (car *unparsed*) (cdr word-list)))
;  (let ((found-word (car *unparsed*)))
;    (set! *unparsed* (cdr *unparsed*))
;    (list (car word-list) found-word)))
;
;(define *unparsed* '())
;(define (parse input)
;  (set! *unparsed* input)
;  (let ((sent (parse-sentence)))
;    (require (null? *unparsed*))
;    sent))
;
;(define prepositions '(prep for to in by with))
;(define (parse-sentence)
;  (list 'sentence
;         (parse-noun-phrase)
;         (parse-verb-phrase)))
;(define (parse-verb-phrase)
;  (define (maybe-extend verb-phrase)
;    (amb verb-phrase
;         (maybe-extend (list 'verb-phrase
;                             verb-phrase
;                             (parse-prepositional-phrase)))))
;  (maybe-extend (parse-word verbs)))
;
;(define (parse-simple-noun-phrase)
;  (list 'simple-noun-phrase
;        (parse-word articles)
;        (parse-word nouns)))
;(define (parse-noun-phrase)
;  (define (maybe-extend noun-phrase)
;    (amb noun-phrase
;         (maybe-extend (list 'noun-phrase
;                             noun-phrase
;                             (parse-prepositional-phrase)))))
;  (maybe-extend (parse-simple-noun-phrase)))

;(parse '(the student with the cat sleeps in the class))
;(parse '(the professor lectures to the student with the cat))
