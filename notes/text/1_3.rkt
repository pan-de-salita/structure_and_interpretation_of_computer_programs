#lang sicp

;;;;;; 1.3 FORMULING ABSTRACTIONS WITH HIGHER-ORDER PROCEDURES

;; procedures are, in effect, abstractions that describe compound operations on data independent of
;; the particular data:

;; e.g.
(define (cube x) (* x x x))
;; does not express the computation of the cube of a particular number, but rather a method for
;; obtaining the cube of any number

;; without ever defining procedures forces us to always work at the the level of the particular
;; operations that happen to be primitives in the language rather than in terms of higher-level
;; operations.

;; HIGHER-ORDER PROCEDURES
;; procedures that manipulate procedures

;;;; 1.3.1 PROCEDURES AS ARGUMENTS

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

;; note the similarities in the three procedures:
;; (define (<name> a b)
;;   (if (> a b)
;;     0
;;     (+ (<term> a)
;;        (<name> (<next> a) b))))
;;
;; with only the following differences:
;; - the name of the procedure
;; - the function of a used to compute the term to be added
;; - the function that provides the next value of a

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (sum-integers-hop a b)
  (sum (lambda (x) x) a (lambda (x) (+ x 1)) b))

(define (sum-cubes-hop a b)
  (sum cube a (lambda (x) (+ x 1)) b))

(define (pi-sum-hop a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(= (sum-integers 1 10) (sum-integers-hop 1 10))
(= (sum-cubes 1 10) (sum-cubes-hop 1 10))
(= (pi-sum 1 10) (pi-sum-hop 1 10))

;; compute an approximation to pi:
(* 8 (pi-sum-hop 1 1000))

;; similar to summation, which allows mathematicians to deal with the CONCEPT of summation itself rather
;; than only with particular sums

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f
          (+ a (/ dx 2.0))
          add-dx
          b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;;;; 1.3.2 CONSTRUCTING PROCEDURES USING LAMBDA

;; lambda is used to create procedures in the same way as define, except that no name is specified for
;; the procedure:
;; (lambda (<formal-parameters>) <body>)

(define (plus4 x) (+ x 4))
;; is equivalent to:
;; (define plus4 (lambda (x) (+ x 4)))

(lambda                        (x)     (+   x     4))
;;  |                           |       |   |     |
;; the procedure of an argument x that adds x and 4

;; like any expression that has a procedure as its value, a lambda expression can be used as the operator
;; in a combination such as:
((lambda (x y z) (+ x y ((lambda (i) (* i i)) z))) 1 2 3)
;; or, more generally, in any context where we would normally use a procedure name

;; USING LET TO CREATE LOCAL VARIABLES

(define square (lambda (x) (* x x)))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))

  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f-lambda x y)
  ((lambda (a b)
       (+ (* x (square a))
          (* y b)
          (* a b))))
  (+ 1 (* x y))
  (- 1 y))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; general form of a let expression:
;; (let ((<var1> <exp1)
;;       (<var2> <exp2)
;;       ...
;;       (<varn> <expn))
;;   <body>)
;;
;; in other words:
;; let <var1> have the value of <exp1> and
;;     <var2> have the value of <exp2> and
;;     ...
;;     <varn> have the value <expn>
;;   in <body>
;;
;; the let expression is interpreted as an alternate syntax for:
;; ((lambda (<var1> ... <varn>)
;;    <body>))
;;  <exp1>
;;  ...
;;  <expn>)

;; NOTE: a let expression is simply syntactic sugar for the underlying lambda application.

;; note the following characteristics:
;; NOTE: 1. let allows one to bind variables as locally as possible to where they are to be used:
(define (test-let1 x)
  (+ (let ((x 3))
       (+ x (* x 10)))
     x))

(define (test-lambda1 x)
  (+ ((lambda (x)
        (+ x (* x 10)))
      3)
     x))

(= (test-let1 5) (test-lambda1 5)) ;; => #t

;; NOTE: 2. the variables' values are computed outside the let. this matters when the expressions that
;; proved the values for the local variables depend upon variables having the same names as the local
;; variables themselves:

(define (test-let2 x)
  (let ((x 3)
        (y (+ x 2)))
    (* x y)))

(define (test-lambda2 x)
  ((lambda (x y)
     (* x y))
   3 (+ x 2)))

(= (test-let2 2) (test-lambda2 2))
