#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))

  (* (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n)) ;; if n is even
  (define y (f (+ a (* k h))))

  (* ...
     (/ h 3)))
