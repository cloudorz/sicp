; number only

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 
                   (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; 最坏的情况是添加的数大于集合中的最大的数，复杂度是O(n) 
; 最好的情况是添加的数小于集合中最小的数 1
; 平均是n/2
(define (adjoin-set x set)
  (cond ((or (null? set) (= x (car set))) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) 
         (cons (car set) (adjoin-set x (cdr set))))))

; O(n) 最坏的情况是两个集合没有交集, m+n
; 最好的两集合相同，n
; 复杂度是 O(n)
(define (union-set set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2))) 
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) 
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) 
                       (cons x2 (union-set set1 (cdr set2)))))))))

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
