(define (element-of-set? x set) 
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define set1 (list 1 2 3))
(define set2 (list 3 4 5))

; #f
(element-of-set? 4 set1)

; #t
(element-of-set? 4 set2)

; (0 1 2 3)
(adjoin-set 0 set1)

; (3 4 5)
(adjoin-set 3 set2)

; (3)
(intersection-set set1 set2)

; (1 2 3 4 5)
(union-set set1 set2)

; #t
(element-of-set? 'x (adjoin-set 'x set1))

; #f
(element-of-set? 'x '())

; #t
(eq? (element-of-set? 'x (union-set set1 set2)) (or (element-of-set? 'x set1) (element-of-set? 'x set2))) 
