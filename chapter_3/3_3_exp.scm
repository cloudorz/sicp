; example
;(define (cons x y)
;    (let ((new (get-new-pair)))
;      (set-car! new x)
;      (set-cdr! new y)
;      new))
;
;
;(cons 1 2)

; 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

; (a b c d)
z

; (b) 
(cdr x)

(define w (append! x y))

; (a b c d)
w

; (b c d)
(cdr x)

; exe 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; infinite cycle
;(last-pair z)


; exe 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

v

w

;(define (cons x y) 
;  (define (set-x! v) (set! x v))
;  (define (set-y! v) (set! y v))
;  (define (dispatch m)
;    (cond ((eq? m 'car) x)
;          ((eq? m 'cdr) y)
;          ((eq? m 'set-car) set-x!)
;          ((eq? m 'set-cdr) set-y!)
;          (else (error "Undefined operation -- CONS" m))))
;  dispatch)
;
;(define (car z) (z 'car))
;(define (cdr z) (z 'cdr))
;(define (set-car! z new-value)
;  ((z 'set-car!) new-value) 
;  z)
;
;(define (set-cdr! z new-value)
;  ((z 'set-cdr!) new-value)
;  z)

; 3.16
(define (count-pairs x) 
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define z3 '(a b c))
z3
(count-pairs z3)
; Value 3

(define x '(a))
(define z4 (list x x))
z4
(count-pairs z4)
; Value 4

(define x '(a))
(define y (cons x x))
(define z7 (cons y y))
z7
(count-pairs z7)
; Value 7

(define zi '(a b c))
(set-cdr! (cddr zi) zi)
(count-pairs zi)

; 3.17
(define (count-pairs x)
  (let ((aux '()))
    (define (count z)
      (cond ((not (pair? z)) 0)
            ((memq z aux) 0)
            (else 
              (if (null? aux)
                (set! aux (list z))
                (set-cdr! (last-pair aux) (list z)))
              (+ (count (car z))
                 (count (cdr z))
                 1))))
    (count x)))

(count-pairs z7)

; 3.18

(define (cycle? x)
  (let ((aux '()))
    (define (find-cycle z)
      (cond ((null? z) false)
            ((memq z aux) true)
            (else 
              (set-cdr! (last-pair aux) (list z))
              (find-cycle (cdr z)))))
    (if (not (pair? x))
      (error "Argument of cycle? must be a pair."))
    (set! aux (list x))
    (find-cycle (cdr x))))

(define (make-cycle x) 
  (set-cdr! (last-pair x) x) 
  x)

(define z1 (make-cycle (list 'a 'b 'c)))
(define z2 (cons 'q z1))

(cycle? z1)

(cycle? z2)

(cycle? (make-cycle '(a)))

(cycle? (list 'a 'b (make-cycle '(1 2 3 4)) 'c))

(define (constraint-cycle? x)
  (define (next-step-one current-p)
    (if (null? current-p)
      current-p
      (cdr current-p)))
  (define (next-step-two current-p)
    (cond ((null? current-p)
           current-p)
          ((null? (cdr current-p))
           '())
          (else 
            (cddr current-p))))
  (let ((p x)
        (p2 x))
    (define (xx)
      (begin (set! p (next-step-one p)) (set! p2 (next-step-two p2)))
      (cond ((or (null? p) (null? p2))
             #f)
            ((eq? p p2)
             #t)
            (else (xx))))
    (xx)))

(constraint-cycle? z2)
(constraint-cycle? (list 'a 'b (make-cycle '(1 2 3 4)) 'c))
