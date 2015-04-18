(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define *+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (*+ n (sub1 m)))))))
(define *-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (*- n (sub1 m)))))))

(define **
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (*+ n (** n (sub1 m)))))))
(define */
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (*/ (*- n m) m))))))

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(define > 
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (> (sub1 n) (sub1 m))))))
(define < 
  (lambda (n m)
    (cond ((zero? n) #t)
          ((zero? m) #f)
          (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (not (or (> n m) (< n m)))))

(define =
  (lambda (n m)
    (cond ((> n m) #f)
          ((< n m) #f)
          (else #t))))

(define ^
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (** n (^ n (sub1 m)))))))

(define one? 
  (lambda (n)
    (cond ((zero? n) #f)
          (else (zero? (sub1 n))))))

(define one?
  (lambda (n)
    (= n 1)))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick n (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (= a1 a2))
          ((or (number? a1) (number? a2))
           #f)
          (else (eq? a1 a2)))))

(define sero? 
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define **+
  (lambda (n m)
    (cond ((sero? m) n)
          (else (edd1 (**+ n (zub1 m)))))))
