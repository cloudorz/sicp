(define (Y F)
  (lambda (x) ((F (Y F)) x)))

((Y (lambda (lengthf)
     (lambda (l)
       (cond ((null? l) 0)
             (else (+ 1 (lengthf (cdr l)))))))) 
 '(1 2 3 4))
