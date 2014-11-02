(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones)) 

(define integers (cons-stream 1 (add-streams ones integers)))
;(1 . #promise)
;(1 . (2 . * (stream-map + (stream-cdr ones) (stream-cdr integers))))
;(1 . (2 . (3 . * (stream-map + (stream-cdr (stream-cdr ones)) (stream-cdr (stream-cdr integers)))


(define fibs
  (cons-stream 0 
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

;(0 . (1 . #promise)
;(0 . (1 . (1 . * (stream-map (stream-cdr (stream-cdr fibs)) (stream-cdr fibs)))))
;(0 . (1 . (1 . (2 . * (stream-map + (stream-cdr (stream-cdr (stream-cdr fibs))) (stream-cdr (stream-cdr fibs)))))))

(define (scale-stream stream factor) 
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))
;(1 . #promise)
;(1 . (2 . * (stream-map xxx (stream-cdr double))))
;(1 . (2 . (4 . * (stream-map xxx (stream-cdr (stream-cdr double))))))

; 3.53
; 1 2 4 8 16 32 64 ..
(define s (cons-stream 1 (add-stream s s)))


; 3.54
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; 3.55
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (integer-starting-from n)
  (cons-stream n (integer-stream-starting (+ n 1))))

(define primes (cons-stream 2 
                            (stream-filter prime? 
                                           (integer-starting-from 3))))
;(define (prime? n)
  ;;;(define (iter ps)
  ;  (cond ((> (square (stream-car ps)) n) #t)
  ;        ((disviable? n (stream-car ps)) #f)
  ;        (else (iter (stream-cdr)))))
  ;(iter primes))

; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else 
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
