; length
(define length 
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l))))))) 

; version 1 length <= 2
(lambda (l)
  (cond ((null? l) 0)
        (else (add1 ((lambda (l)
                       (cond ((null? l) 0))
                       (else (add1 ((lambda (l)
                                      (cond ((null? l) 0)
                                            (else (add1 (eternity (cdr l))))))
                                    (cdr l)))))
                     (cdr l))))))

; version 2 

; length = 0
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
 eternity)

; length <= 1
((lambda (f)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (f (cdr l)))))))
 ((lambda (g)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (g (cdr l)))))))
  eternity))

; llength <= 2
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
  ((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
   eternity)))

; version 3
; legnth = 0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l))))))))

; length <= 3
((lambda (mk-length)
   (mk-length 
     (mk-length 
       (mk-length 
         (mk-length 
           (mk-length eternity))))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l))))))))

; version 4
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 ((mk-length mk-length) 
                        (cdr l))))))))

; step 0
((lambda (mk-length)
   (mk-length mk-length)) 
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

; setp 1
((lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    (mk-length mk-length)))
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

; step 2
((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    ((lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond ((null? l) 0)
                  (else (add1 (length (cdr l)))))))
        (mk-length mk-length)))
     (lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond ((null? l) 0)
                  (else (add1 (length (cdr l)))))))
        (mk-length mk-length)))))

; step 3
((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    ((lambda (length)
          (lambda (l)
            (cond ((null? l) 0)
                  (else (add1 (length (cdr l)))))))
        ((lambda (mk-length)
           ((lambda (length)
              (lambda (l)
                (cond ((null? l) 0)
                      (else (add1 (length (cdr l)))))))
            (mk-length mk-length)))
         (lambda (mk-length)
           ((lambda (length)
              (lambda (l)
                (cond ((null? l) 0)
                      (else (add1 (length (cdr l)))))))
            (mk-length mk-length))))))

; Y combinator
; step 1
((lambda (mk-length)
   (mk-length mk-length)) 
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))

; step 2
((lambda (mk-length)
   (mk-length mk-length)) 
 (lambda (mk-length)
   ((lambda (length)
     (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l)))))))
    (lambda (x) 
      ((mk-length mk-length) x)))))

; step 3
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x) 
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l))))))))

; step 4
(define Y 
  (lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
      (le (lambda (x) ((f f) x)))))))

(define (Y F)
  (lambda (x) ((F (Y F)) x))
