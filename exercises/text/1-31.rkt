#lang sicp

;; Number (Number -> Number) Number (Number -> Number) -> Number
;; returns the product of the values of a function at points over a given range
(define (product term a next b)
  (if (> a b)
    1.0
    (* (term a) (product term (next a) next b))))

;; Number -> Number
;; returns the factorial of a given number
(define (factorial i)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) i))

(factorial 5)
(= (factorial 5) 120)
