(define (front-ptr queue) 
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item) 
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else 
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

; 3.21

(define (print-queue queue)
  (front-ptr queue))

(define q1 (make-queue))

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)
;; 解释: 因为系统打印出的指向头尾的序对，car是整个的队列，cdr部分是最后一个序对。应该是打印 (front-ptr queue)

; 3.22

(define (make-queue-p)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" queue)
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else 
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" queue))
            (else 
              (set! front-ptr (cdr front-ptr)))))
    (define (print-queue)
      front-ptr)
    (define (dispath m)
      (cond ((equal? m 'empty-queue?) empty-queue?)
            ((equal? m 'front-queue) front-queue)
            ((equal? m 'insert-queue!) insert-queue!)
            ((equal? m 'delete-queue!) delete-queue!)
            ((equal? m 'print-queue) print-queue)
            (else 
              (error "Method not included" m))))
    dispath))

(define (empty-queue-p? queue)
  ((queue 'empty-queue?)))

(define (front-queue-p queue)
  ((queue 'front-queue)))

(define (insert-queue-p! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue-p! queue)
  ((queue 'delete-queue!)))

(define (print-queue-p queue)
  ((queue 'print-queue)))

(define q2 (make-queue-p))

(insert-queue-p! q2 'a)
(print-queue-p q2)

(insert-queue-p! q2 'b)
(print-queue-p q2)

(delete-queue-p! q2)
(print-queue-p q2)

(delete-queue-p! q2)
(print-queue-p q2)

; 3.23
(define (make-deque) (cons '() '()))

(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty queue" deque)
    (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty queue" deque)
    (car (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else 
            (set-car! (cdr new-pair) (rear-ptr deque))
            (set-cdr! (cdr (rear-ptr deque)) new-pair)
            (set-rear-ptr! deque new-pair)))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else 
            (set-cdr! (cdr new-pair) (front-ptr deque))
            (set-car! (cdr (front-ptr deque)) new-pair)
            (set-front-ptr! deque new-pair)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else 
          (set-rear-ptr! deque (cadr (rear-ptr deque)))
          (set-cdr! (cdr (rear-ptr deque)) '()))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else 
          (set-front-ptr! deque (cddr (front-ptr deque)))
          (set-car! (cdr (front-ptr deque)) '()))))

(define (print-deque deque)
  (define (make-printable-list q)
    (if (null? q)
      '()
      (cons (car q) 
            (make-printable-list (cddr q)))))
  (newline)
  (display (make-printable-list (front-ptr deque))))

; test
(define q1 (make-deque))
(front-insert-deque! q1 'a)
(print-deque q1)
; (a)
(front-insert-deque! q1 'b)
(print-deque q1)
; (b a)
(rear-insert-deque! q1 'x)
(print-deque q1)
; (b a x)
(rear-insert-deque! q1 'y)
(print-deque q1)
; (b a x y)
(rear-delete-deque! q1)
(print-deque q1)
; (b a x)
(front-delete-deque! q1)
(print-deque q1)
; (a x)
(front-delete-deque! q1)
(print-deque q1)
; (x)
(front-delete-deque! q1)
(print-deque q1)
; ()
(empty-deque? q1)
;Value: #t

(define q2 (make-deque))
(rear-insert-deque! q2 1)
(print-deque q2)
; (1)
(front-insert-deque! q2 3)
(print-deque q2)
; (3 1)
(front-insert-deque! q2 5)
(print-deque q2)
; (5 3 1)
(front-deque q2)
;Value: 5
(rear-deque q2)
;Value: 1
(front-delete-deque! q2)
(print-deque q2)
; (3 1)
(front-deque q2)
;Value: 3
(rear-deque q2)
;Value: 1
(rear-delete-deque! q2)
(print-deque q2)
; (3)
(front-deque q2)
;Value: 3
(rear-deque q2)
;Value: 3
(empty-deque? q2)
;Value: #f
(rear-delete-deque! q2)
(print-deque q2)
; ()
(empty-deque? q2)
;Value: #t
