#lang sicp

;; Number (Number -> Number) Number (Number -> Number) -> Number
;; returns the sum of the values of a function at points over a given range
(define (sum term a next b)
  (if (> a b)
    0.0
    (+ (term a) (sum term (next a) next b))))

;; Number (Number -> Number) Number (Number -> Number) -> Number
;; returns the product of the values of a function at points over a given range
(define (product term a next b)
  (if (> a b)
    1.0
    (* (term a) (product term (next a) next b))))

;; (Number Number -> Number) Number Number (Number -> Number) Number (Number -> Number)
;; returns a procedure that accumulates values of a function at points over a given range
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (sum-from-accumulate term a next b)
  (accumulate + 0.0 term a next b))

(define (product-from-accumulate term a next b)
  (accumulate * 1.0 term a next b))

(= (sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
   (sum-from-accumulate (lambda (x) x) 1 (lambda (x) (+ x 1)) 10))

(= (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
   (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 10))
