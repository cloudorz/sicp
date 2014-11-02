; 3.1.1 P.153 example
(define (make-account balance) 
  (define (withdraw amount) 
    (if (>= balance amount) 
      (begin (set! balance (- balance amount)) 
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m) 
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MARK-ACCOUNT" 
                       m))))
  dispatch)

(define acc (make-account 100))

; 50
((acc 'withdraw) 50)

; Insufficient funds
((acc 'withdraw) 60)

; 90
((acc 'deposit) 40)

; 30
((acc 'withdraw) 60)

; execise 3.1

(define (make-accumulator init-number) 
  (lambda (delta) 
    (begin (set! init-number (+ init-number delta))
           init-number)))

(define A (make-accumulator 5))

; 15
(A 10)

; 25
(A 10)

; execise 3.2
(define (make-monitered f)
  (let ((count 0)) 
    (lambda (sign)
      (cond ((eq? sign 'how-many-calls?) count)
            ((eq? sign 'reset-count)
             (set! count 0))
            (else (begin (set! count (+ count 1)) (f sign))))))) ; bug: the sign may be other symbol

(define s (make-monitered sqrt))

; 10
(s 100)

; 1
(s 'how-many-calls?)

(s 10)
(s 'how-many-calls?)

(s 'reset-count)
(s 'how-many-calls?)

; execise 3.3
(define (make-account balance passwd) 
  (define (withdraw amount) 
    (if (>= balance amount) 
      (begin (set! balance (- balance amount)) 
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops) 
    (error "Incorrect password policmen, the thief, come pls."))
  (let ((error-passwd-count 0)) 
    (lambda (p m) 
      (if (eq? p passwd) 
        (begin (set! error-passwd-count 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     ((eq? m 'joint) True)
                     (else (error "Unknown request -- MARK-ACCOUNT" m))))
        (begin (set! error-passwd-count (+ error-passwd-count 1))
               (if (> error-passwd-count 7) 
                 (call-the-cops)
                 (error "Incorrect password")))))))

(define acc (make-account 100 'secret-password))

; 60
((acc 'secret-password 'withdraw) 40)

; Incorrect password
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)
;((acc 'some-other-password 'deposit) 50)

; 3.1.2

; examp monte carlo
(define (rand-update x) 
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 123456))
    (modulo (+ (* a x) c) m)))

(define rand-init 10)
(define rand 
  (let ((x rand-init))
    (lambda () 
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test) 
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment) 
  (define (iter trials-remaining trials-passed) 
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment) 
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else 
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(estimate-pi 1000)

; execise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral x1 x2 y1 y2 trials) 
  (let ((r (/ (- x2 x1) 2))
        (area (* (- x2 x1) (- y2 y1)))
        (percent (monte-carlo trials (make-predicate x1 x2 y1 y2))))
    (/ (* area percent) (square r))))

(define (make-predicate x1 x2 y1 y2)
  (lambda () 
    (let ((r_x (random-in-range x1 x2))
          (r_y (random-in-range y1 y2))
          (c_x (+ x1 (/ (- x2 x1) 2)))
          (c_y (+ y1 (/ (- y2 y1) 2)))
          (r (/ (- x2 x1) 2)))
      (<= (+ (square (- r_x c_x))
             (square (- r_y c_Y))) 
          (square r)))))

(estimate-integral 2 10002 4 10004 100000)

; exe 3.6
(define random-init 120)
(define randx
  (let ((x random-init))
    (lambda (sym) 
      (cond ((eq? sym 'generate) 
             (begin (set! x (rand-update x)) 
                    x))
            ((eq? sym 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else 
              (error "unknow request"))))))

(randx 'generate)

(define y (randx 'generate))

(randx 'generate)

((randx 'reset) y)

(randx 'generate)

; exe 3.7
(define (make-joint account account-passwd new-passwd)
  (if (account account-passwd 'joint)
    (lambda (passwd sym) 
      (if (eq? passwd new-passwd) 
        (account account-passwd sym)
        (error "Incorrect Password")))
    (error "Wrong Password can't joint")))

(define peter-acc 
  (make-account 100 'open-sesame))

(define paul-acc 
  (make-joint peter-acc 'open-sesame 'rosebund))

((paul-acc 'rosebund 'withdraw) 60)

((paul-acc 'rosebund 'deposit) 35)

(define ruce-acc 
  (make-joint paul-acc 'rosebund 'xxpp))

((ruce-acc 'xxpp 'withdraw) 15)

((peter-acc 'open-sesame 'withdraw) 25)

((paul-acc 'rosebund 'deposit) 150)

(define bad-man-acc 
  (make-joint ruce-acc 'px 'pass))

; exe 3.8

(define f 
  (let ((init 1))
    (lambda (x) 
      (begin (set! init (* init x)) 
             init))))

;(f 0)
;(f 1)

(f 1)
(f 0)

; (+ (f 0) (f 1))

