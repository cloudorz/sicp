(load "prime.scm")
(load "stream_adt.scm")

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;(stream-car (stream-cdr (stream-filter prime?
;                                       (stream-enumerate-interval 10000 1000000))))
