; 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc2 key records)
      (cond ((null? records)
             #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc2 key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc2 key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc2 key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc2 key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc2 key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable 
                        (cons (cons key-2 value) 
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.25
(define (make-m-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc2 key records)
      (cond ((or (null? records) (not (pair? records)))
             #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc2 key (cdr records)))))
    (define (lookup keys)
      (define (lookup-with-table subkeys subtable)
        (let ((record (assoc2 (car subkeys) (cdr subtable))))
          (if record
            (if (null? (cdr subkeys))
              (cdr record)
              (lookup-with-table (cdr subkeys) record))
            #f)))
      (lookup-with-table keys local-table))
    (define (insert! keys value)
      (define (make-part-table subkeys)
        (if (null? subkeys)
          value
          (cons (cons (car subkeys) (make-part-table (cdr subkeys))) '())))
      (define (insert-with-table! subkeys subtable)
        (let ((record (assoc2 (car subkeys) (cdr subtable))))
          (if record
            (if (null? (cdr subkeys))
              (set-cdr! record value)
              (if (pair? (cdr record))
                (insert-with-table! (cdr subkeys) record)
                (error "INSERT! key have a value" subkeys)))
            (let ((part-table (make-part-table subkeys)))
              (set-cdr! part-table (cdr subtable))
              (set-cdr! subtable part-table)))))
      (insert-with-table! keys local-table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 3.27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f) 
  (let ((table (make-m-table equal?)))
    (lambda (x)
      (let ((previously-computed-result ((table 'lookup-proc) (list x))))
        (or previously-computed-result
            (let ((result (f x)))
              ((table 'insert-proc) (list x) result)
              result))))))

(define memo-fib 
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
