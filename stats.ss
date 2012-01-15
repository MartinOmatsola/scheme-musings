#lang scheme

(define (mean lst)
  (if (empty? lst) null (/ (apply + lst) (length lst))))

(define (variance lst)
  (if (empty? lst) null
      (let* ([xbar (mean lst)]
             [var-lst (map (lambda (y) (expt (- y xbar) 2)) lst)]) 
        (/ (apply + var-lst) (- (length lst) 1)))))
  
(define (standard-deviation lst)
  (if (empty? lst) null (sqrt (variance lst))))

(define (covariance lst1 lst2)
  (define xbar (mean lst1))
  (define ybar (mean lst2))
  ( / (foldl (lambda (a b result)
               (+ result (* (- a xbar) (- b ybar))))
             0
             lst1 
             lst2)
      (- (length lst1) 1)))