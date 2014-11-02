(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficent funds"))

(define new-withdraw 
  (let ((balance 100))
    (lambda (amount) 
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficent funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficent funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)

(W2 70)

(W2 40)

(W1 40)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficent funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknow request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

((acc 'withdraw) 50)

((acc 'withdraw) 60)

((acc 'deposit) 40)

((acc 'withdraw) 60)

;; ex 3.1
(define (make-accumulator first-number)
  (lambda (delta)
    (begin (set! first-number (+ first-number delta)) 
           first-number)))

(define A (make-accumulator 5))

(A 10)

(A 10)
