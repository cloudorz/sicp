(load "stream_utils")

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map (cons proc (map stream-cdr argstreams))))))

; 3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
(stream-ref x 5)
; 1
; 2
; 3
; 4
; 5

(stream-ref x 7)
; 6
; 7

; 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

; sum = 1
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-line sum)
; sum = 6
(define y (stream-filter even? seq))
(display-line sum)

; sum = 10
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display-line sum)
; 136
(stream-ref y 7)
(display-line sum)

; 210
(display-stream z)
(display-line sum)

; 如果不做memo－proc的话，会有很大的不同，因为在stream-map  delay ((accsum a) delay ((accsum b) ((accsum c) ...)))
