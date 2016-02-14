;; DrRacket R5RS

;; 2.4 
;; Exercise 2.4.  Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2a 3b. Give the corresponding definitions of the procedures cons, car, and cdr.
(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))
(define (car z)
  (if (= 0 (remainder z 2))
    (+ 1 (car (/ z 2)))
    0))
(define (cdr z)
  (if (= 0 (remainder z 3))
    (+ 1 (cdr (/ z 3)))
    0))

;; 2.6
;; Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

;; (define zero (lambda (f) (lambda (x) x)))

;; (define (add-1 n)
;;  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the  calculus.

;; Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)   (lambda (f) (lambda (x) (f ((n f) x)))))

(define one   (lambda (f) (lambda (x) (f x))))

(define two   (lambda (f) (lambda (x) (f (f x)))))

(define (add n1 n2) (lambda (f) (lambda (x) ((n1 f) ((n2 f) x)))))

(define (ch-print n) ((n (lambda (x) (display '1))) '0))

