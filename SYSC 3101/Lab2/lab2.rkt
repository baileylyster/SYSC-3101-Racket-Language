#lang racket

(define (sum-number numbers)
        (cond
          [(empty? numbers) 0] 
          [else(+ (car numbers)(sum-number(cdr numbers)))]
          ))



(define (average numbers)
  (/ (sum-number numbers) (length numbers)))


(define (occurrences numbers n)
  (cond
    [(empty? numbers) 0]
    [(equal? n (car numbers)) (+ (occurrences (cdr numbers) n) 1)]
    [else (+  (occurrences (cdr numbers) n) 0)]))
   

(define (convert lst)
  (convert-list-helper (reverse lst)))

(define (convert-list-helper lst)
  (cond
    [(empty? lst) 0]
    [else (+ (*(car lst) (expt 10 (- (length lst) 1 ))) (convert-list-helper (cdr lst)))]
    ))

(define (convertFC tempF)
  (convertFC-helper tempF))


(define (convertFC-helper lst)
  (cond
          [(empty? lst)]
          [else(append (list (* (- (car lst) 32) 0.555)) (convertFC-helper (cdr lst )))]
          ))


          
          
                              
  
             
  
  






  
 
        
        

 
    
    
  



          






