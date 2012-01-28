#lang scheme         
(require "stats.ss")
(provide (all-defined-out))

;; calculate real risk free rate given the nominal
;; risk free rate of return and inflation rate
(define (rrfr nrfr inflation-rate)
  (- (/ (+ 1 nrfr) (+ 1 inflation-rate)) 1))