;; PS: run on DrRacket R5Rs

(define (triangle-num n i) 
     (cond ((or (< i 0) (>= i n)) 0)
           ((or (= i 0) (= (- n 1) i)) 1)
           (else (+ (triangle-num (- n 1) (- i 1)) (triangle-num (- n 1) i)))))

(define (factorial n)
        (define (iter product counter)
                (if (> counter n)
                        product
                        (iter (* counter product) (+ counter 1))
                ))
        (iter 1 1))

(define (triangle-num-2 n i)
  (/ (factorial n) (* (factorial (- n i)) (factorial i))))
