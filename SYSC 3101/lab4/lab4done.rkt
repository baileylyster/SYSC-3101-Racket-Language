#lang racket

;; SYSC 3101 A Winter 2022 Lab 4

;; Exercise 1
;Step 1, nothing happens when the first expression. running the expression following increments from 0(start),1,2.
;The objects are independent
;((make-up counter 0)) does not work

(define (make-upcounter counter)
  (lambda () 
    (set! counter (+ counter 1))
    counter))


;; Exercise 2
;procedure dispatch is bound to counter3
;'inc count-up
;'dec count-down
;'reset unknown command
;inc and dec increase and decrease and tell the tally, reset is still unknown.


(define (make-counter counter)
  
  (define (count-up) 
    (set! counter (+ counter 1))
    counter)
  
  (define (count-down)
    (if (> counter 0)
        (begin (set! counter (- counter 1))
               counter)
        "Counter is 0"))

  (define (dispatch cmd)
    (cond ((eq? cmd 'inc) count-up)
          ((eq? cmd 'dec) count-down)
          (else (error "Unknown command:" cmd))))
  
  dispatch)


;; Exercise 3

(define (make-counter-with-let initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (dispatch cmd)
      (cond ((eq? cmd 'inc) count-up)
            ((eq? cmd 'dec) count-down)
            (else (error "Unknown command:" cmd))))
 
    dispatch))

;;Exercise 4
(define (make-counter-ex4 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))
    (lambda (cmd) (
                   (cond ((eq? cmd 'inc) count-up)
            ((eq? cmd 'dec) count-down)
            (else (error "Unknown command:" cmd)))))))

;;Exercise 5
(define (make-counter-ex5 initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
    (define (get-counter)
      counter)
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))
    (define (reset-counter)
      (set! counter 0))
    (lambda (cmd) 
                   (cond
                     ((eq? cmd 'inc) count-up)
                     ((eq? cmd 'dec) count-down)
                     ((eq? cmd 'get) get-counter)
                     ((eq? cmd 'reset)reset-counter)
                     (else (error "Unknown command:" cmd))))))

;;Exercise 6
(define (make-counter-ex6 initial-count stepsize)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter stepsize))
      counter)
    (define (get-counter)
      counter)
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))
    (define (reset-counter)
      (set! counter 0))
    (lambda (cmd) 
                   (cond
                     ((eq? cmd 'inc) count-up)
                     ((eq? cmd 'dec) count-down)
                     ((eq? cmd 'get) get-counter)
                     ((eq? cmd 'reset) reset-counter)
                     (else (error "Unknown command:" cmd))))))

;;Exercise 7
(define (make-counter-ex7 initial-count stepsize) 
  (let ((counter initial-count))

    (define mx 0)
    (define (get-counter)
      counter)
    (define (count-up)
      (set! counter (+ counter stepsize))
      counter)
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))
    (define (find-max)
      (if (> counter mx)
          (set! mx counter)
           mx)mx)
    (define (reset-counter)
      (set! counter 0))
    (lambda (cmd) 
                   (cond
                     ((eq? cmd 'inc) count-up)
                     ((eq? cmd 'dec) count-down)
                     ((eq? cmd 'get) get-counter)
                     ((eq? cmd 'max) find-max)
                     ((eq? cmd 'reset) reset-counter)
                     (else (error "Unknown command:" cmd))))))
   

  










