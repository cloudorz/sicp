;(define (rand-update x)
;  (let ((a (expt 2 32))
;        (c 1103515245)
;        (m 12345))
;    (modulo (+ (* a x) c) m)))
;(define random-init 137)
;(define rand 
;  (let ((x random-init))
;    (define (dispatch m)
;      (cond ((eq? m 'generate)
;             (begin (set! x (rand-update x))
;                    x))
;            ((eq? m 'reset) 
;             (lambda (new-x)
;               (set! x new-x)))
;            (else (error "unknown request"))))
;    dispatch))
;
;; test
;(rand 'generate)
;;Value: 3062
;(rand 'generate) 
;;Value: 1397
;((rand 'reset) 3062)
;(rand 'generate)
;;Value: 1397

(define (hefty-computation do-other-stuff) 
    (let loop ((n 5)) 
      (display "Hefty computation: ") 
      (display n) 
      (newline) 
      (set! do-other-stuff (call-with-current-continuation do-other-stuff)) 
      (display "Hefty computation (b)")  
      (newline) 
      (set! do-other-stuff (call-with-current-continuation do-other-stuff)) 
      (display "Hefty computation (c)") 
      (newline) 
      (set! do-other-stuff (call-with-current-continuation do-other-stuff)) 
      (if (> n 0) 
          (loop (- n 1)))))

 ;; notionally displays a clock 
 (define (superfluous-computation do-other-stuff) 
    (let loop () 
      (for-each (lambda (graphic) 
                  (display graphic) 
                  (newline) 
                  (set! do-other-stuff (call-with-current-continuation do-other-stuff))) 
                '("Straight up." "Quarter after." "Half past."  "Quarter til.")) 
      (loop)))

(define call/cc call-with-current-continuation)
(define the-continuation #f)

(define (test)
(let ((i 0))
 ; call/cc calls its first function argument, passing
 ; a continuation variable representing this point in
 ; the program as the argument to that function.
 ;
 ; In this case, the function argument assigns that
 ; continuation to the variable the-continuation.
 ;
 (call/cc (lambda (k) (set! the-continuation k)))
 ;
 ; The next time the-continuation is called, we start here.
 (set! i (+ i 1))
 i))
