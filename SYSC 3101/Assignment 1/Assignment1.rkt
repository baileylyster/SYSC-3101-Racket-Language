#lang racket
;SYSC3101 Bailey Lyster 101115419


;Largely based on my solution to count-occurences from the lab 2, with checking for remainder = 0 since any number with a remainder
; equal to zero must be a multiple of that number
(define (count-multiples numbers n)
  (cond
    [(empty? numbers) 0]
    [(equal? 0 (remainder (car numbers) n)) (+ (count-multiples (cdr numbers) n) 1)]
    [else (+  (count-multiples (cdr numbers) n) 0)]))


;Using for/fold we can easily keep track of a counted value as the list is interated
(define (count-multiples-iter numbers n)
  (for/fold ([x 0])
    ([i numbers])
    (if (equal? 0 (remainder i n)) (add1 x)
        x)))


;I attempted firstly by attempting to solve by using an iterative/recursive approach by
;looking at length of the list then doing a loop over the length of that then looking at each elem and recursively checking again
;however this approach was too challenging
;if i used append instead I would loose the nesting of the list. ie: (deep-list-remove (lambda (x) (< x 4)) '(7 2 (3 4 (5 6)))) -> '(7 4 5 6) when it expects ('(7 (4 (5 6)))
(define (deep-list-remove condition numbers)
  (cond
    [(null? numbers) null]
    [(list? (car numbers)) ;If not the most nested elem
       (cons (deep-list-remove condition (car numbers)) (deep-list-remove condition (cdr numbers)))]
    [else
     (cond
       [(condition (car numbers)) (deep-list-remove condition (cdr numbers))] ;Since it meets the condition, cut off first entry and reenter recursion without it
       [else (cons (car numbers) (deep-list-remove condition (cdr numbers)))] ;Does not meet condition, re-append and countinue recursion on unchecked entries in the list
       )]))
     
      
           
