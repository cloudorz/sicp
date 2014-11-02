(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate d e s)
    (inverter c e)
    (and-gate a b c)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else 
          (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (if (and (or (= a 0) (= a 1)) (or (= b 0) (= b 1)))
    (if (and (= a 1) (= b 1))
      1
      0)
    (error "Invalid signal" (list a b))))

; 3.28
(define (or-gate a1 a2 ouput)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! ouput new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b) 
  (logical-not (logical-and a b)))

; 3.29
; or-gate-delay = and-get-delay + 2 * inverter-delay
;(define (or-gate a1 a2 output)
;  (let ((s (make-wire))
;        (not-a1 (make-wire))
;        (not-a2 (make-wire)))
;    (inverter a1 not-a1)
;    (inverter a2 not-a2)
;    (and-gate not-a1 not-a2 s)
;    (inverter s output)))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

; a simple 
(define (probe name wire)
  (add-action! wire 
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

; queue
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

; agenda
(define (make-time-segement time queue)
  (cons time queue))
(define (segement-time s) (car s))
(define (segement-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) 
  (set-car! agenda time))
(define (segements agenda) (cdr agenda))
(define (set-segements! agenda segements)
  (set-cdr! agenda segements))
(define (first-segement agenda) (car (segements agenda)))
(define (rest-segements agenda) (cdr (segements agenda)))
(define (empty-agenda? agenda)
  (null? (segements agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segements)
    (or (null? segements)
        (< time (segement-time (car segements)))))
  (define (make-new-time-segement time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segement time q)))
  (define (add-to-segements! segements)
    (if (= (segement-time (car segements)) time)
      (insert-queue! (segement-queue (car segements))
                     action)
      (let ((rest (cdr segements)))
        (if (belongs-before? rest)
          (set-cdr! segements
                    (cons (make-new-time-segement time action)
                          (cdr segements)))
          (add-to-segements! rest)))))
  (let ((segements (segements agenda)))
    (if (belongs-before? segements)
      (set-segements! agenda
                      (cons (make-new-time-segement time action)
                            segements))
      (add-to-segements! segements))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segement-queue (first-segement agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segements! agenda (rest-segements agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segement agenda)))
      (set-current-time! agenda (segement-time first-seg))
      (front-queue (segement-queue first-seg)))))

; init values
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)

(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)

(propagate)
