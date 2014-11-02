; memq
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; 2.53

; (a b c) 
(list 'a 'b 'c)
; ((george))
(list (list 'george)) 
; ((y1 y2))
(cdr '((x1 x2) (y1 y2))) 
; (x1 x2)
(car '((x1 x2) (y1 y2))) 
; #f
(pair? (car '(a short list))) 
; #f
(memq 'red '((red shoes) (blue socks))) 
; (red shoes blue socks)
(memq 'red '(red shoes blue socks)) 

; 2.54
(define (equal? x y) 
  (if (and (pair? x) (pair? y)) 
    (let ((a (car x))
          (b (car y)))
      (and ((if (and (pair? a) (pair? b)) equal? eq?) a b)
           (equal? (cdr x) (cdr y))))
    (and (null? x) (null? y))))

; #t
(equal? '(this is a list) '(this is a list))
; #f
(equal? '(this is a list) '(this (is a) list))
