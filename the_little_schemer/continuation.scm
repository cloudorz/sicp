; curring
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) (quote ()))
            ((eq? (car l) old)
             (seq new old (cdr l)))
            (else (cons (car l) 
                        ((inser-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertL (insert-g (lambda (new old l)
                            (cons new (cons old l)))))

(define insertR (insert-g seqR))
(define inserR (insert-g (lambda (new old l)
                           (cons old (cons new l)))))

(define subst (insert-g seqS))

; value eval
(define atom-to-function
  (lambda (x)
    (cond ((eq? x (quote +)) p+)
          ((eq? x (quote *)) p*)
          (else p^))))
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp))
                 (value (1st-sub-exp nexp)
                        (2nd-sub-exp nexp)))))))
; multirember
(define multiremver-f
  (lambda (tes?)
    (lambda (a lat)
      (cond ((null? lat) (quote ()))
            ((test? a (car lat))
             ((multiremver-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multiremver-f test?) (cdr lat))))))))
(define multiremver-eq? (multiremver-f test?))

; collector

(define multiinsertLRf
  (lambda (new oldL oldR lat col)
    (cond 
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLRf new oldL oldR
                       (cdr lat)
                       (lambda (newlat L R)
                         (col (cons new (cons oldL newlat))
                              (+ L 1) R))))
      ((eq? (car lat) oldR)
       (multiinsertLRf new oldL oldR
                       (cdr lat)
                       (lambda (newlat L R)
                         (col (cons oldR (cons new newlat))
                              L (+ R 1)))))
      (else
        (multiinsertLRf new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat)
                               L R)))))))

(multiinsertLRf 'salty 'fish 'chips '(chips and fish or fish and chips))

(define evens-only*
  (lambda (l)
    (cond 
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond 
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l)))
          (else (evens-only* (cdr l))))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(define evens-only*c 
  (lambda (l col)
    (cond ((null? l)
           (col (quote ()) 1 0))
          ((atom? (car l))
           (cond 
             ((even? (car l))
              (evens-only*c (cdr l)
                            (lambda (newlat p s)
                              (col (cons (car l) newlat)
                                   (* (car l) p)
