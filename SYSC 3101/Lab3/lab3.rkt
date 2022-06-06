#lang racket


(define (build-naturals n)
  (build-list n (lambda (x) (+ x 0)))
  )

(define (build-rationals n)
  (build-list n (lambda (x) (/ 1 (+ x 1)))))

(define (build-evens n)
  (build-list n (lambda (x) (* x 2))))





(define (cubic a b c)
  (lambda (x) (+(expt x 3)) (+(* b x ) c)))

(define (twice f)
  (lambda (x) (f (f x)))
)


(define (square x ) (* x x))
(define (inc x) (+ x 1))
