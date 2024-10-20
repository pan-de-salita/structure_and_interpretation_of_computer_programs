#lang racket
(require test-engine/racket-tests)

;; 1.1
;; below is a sequence of expressions. what is the result printed by the interpreter in response to
;; each expression? assume that the sequence is to be evaluated in the order in which it is presented.

10 ;; => 10
(+ 5 3 4) ;; => 12
(- 9 1) ;; => 8
(/ 6 2) ;; => 3
(+ (* 2 4) (- 4 6)) ;; => (+ 8 -2) => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; Initial expression
(+ a b (* a b))
;; Substitute definitions for a and b
;; => (+ 3 (+ a 1) (* 3 (+ a 1)))
;; Substitute b (which is (+ a 1))
;; => (+ 3 (+ 3 1) (* 3 (+ 3 1)))
;; Evaluate the inner addition for b
;; => (+ 3 4 (* 3 4))
;; Evaluate multiplication
;; => (+ 3 4 12)
;; Final addition
;; => (+ 3 (+ 4 12))
;; => (+ 3 16)
;; => 19
(= a b)
;; => (= 3 (+ a 1))
;; => (= 3 (+ 3 1))
;; => (= 3 4)
;; => #f
(if (and (> b a) (< b (* a b))) b a)
;; => (if (and (> (+ a 1) 3) (< b (* a b))) b a)
;; => (if (and (> (+ 3 1) 3) (< b (* a b))) b a)
;; => (if (and (> 4 3) (< b (* a b))) b a)
;; => (if (and #t (< b (* a b))) b a)
;; => (if (and #t (< (+ a 1) (* 3 (+ a 1)))) b a)
;; => (if (and #t (< (+ 3 1) (* 3 (+ 3 1)))) b a)
;; => (if (and #t (< 4 (* 3 4))) b a)
;; => (if (and #t (< 4 12)) b a)
;; => (if (and #t #t) b a)
;; => b
;; => (+ a 1)
;; => (+ 3 1)
;; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; =>
;; (cond ((= 3 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond (#f 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond ((= (+ a 1) 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond ((= (+ 3 1) 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond ((= 4 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond ((= 4 4) (+ 6 7 a))
;;       (else 25))
;; =>
;; (cond (#t (+ 6 7 a))
;;       (else 25))
;; =>
;; (+ 6 7 a)
;; (+ 6 7 3)
;; 16
(+ 2 (if (> b a) b a))
;; => (+ 2 (if (> (+ a 1) 3) b a))
;; => (+ 2 (if (> (+ 3 1) 3) b a))
;; => (+ 2 (if (> 4 3) b a))
;; => (+ 2 (if #t b a))
;; => (+ 2 (if #t (+ a 1) a))
;; => (+ 2 (if #t (+ 3 1) a))
;; => (+ 2 (if #t 4 a))
;; => (+ 2 4)
;; => 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; => 16

;; 1.2
;; translate the following expressions into prefix form:
;; 5 + 4 + (2 - (3 - (6 + 4/5))) / 3(6 - 2)(2 - 7)

(/
  (+ 5 4
     (- 2
        (- 3
           (+ 6 4/5))))
  (* 3
     (- 6 2)
     (- 2 7)))

;; 1.3
;; define a procedure that takes three numbers as arguments and returns the sum of the squares of
;; the two largest numbers.

(define (square x) (* x x))

;; iter 1:
;; (define (sum-of-two-largest-squared x y z)
;;   (cond [(and (<= x y) (<= x z)) (+ (square y) (square z))]
;;         [(and (<= y x) (<= y z)) (+ (square x) (square z))]
;;         [else (+ (square x) (square y))]))

;; iter 2:
(define (sum-of-two-largest-squared sm md lg)
  (cond [(> sm md) (sum-of-two-largest-squared md sm lg)]
        [(> md lg) (sum-of-two-largest-squared sm lg md)]
        [else (+ (square md) (square lg))]))

(check-expect (sum-of-two-largest-squared 0 1 2) 5)
(check-expect (sum-of-two-largest-squared 1 0 2) 5)
(check-expect (sum-of-two-largest-squared 0 2 1) 5)
(check-expect (sum-of-two-largest-squared 1 2 0) 5)
(check-expect (sum-of-two-largest-squared 2 2 2) 8)
(check-expect (sum-of-two-largest-squared 2 1 2) 8)

;; 1.4
;; observe that our model of evaluation allows for combinations whose operators are compound
;; expressions. use this observation to describe the behavior of the following procedure:
;;
;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))

;; following applicative-order evaluation, all subexpressions are evaluated before procedure
;; application. with this in mind, (if (> b 0) + -) is evaluated when evoking a-plus-abs-b, which,
;; depending on whether b is a positive or negative number, returns a primitive operator (+ or -).
;; the returned primitive operator is then applied to the arguments a and b.

;; 1.5
;; ben bitdiddle has invented a test to determine whether the interpreter he is faced with is
;; using applicative-order evaluation or normal-order evaluation. he defines the following two
;; procedures:
;;
;; (define (p) (p))
;;
;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))
;;
;; then he evaluates the expression
;;
;; (test 0 (p))
;;
;; what behavior will ben observe with an interpreter that uses applicative-order evaluation? what
;; behavior will be observe with an interpreter that uses normal-order evaluation? explain your answer.
;; (assume that the evaluation rule for the special form if is the same whether the interpreter is
;; using normal or applicative order: the predicate expression is evaluated first, and the result
;; determines whether to evaluate the consequent or the alternative expression.)

;; with an interpreter using applicative-order evaluation, (p) is evaluated before it is substituted
;; for y in the body of the test function. this is because, in applicative-order evaluation,
;; subexpressions are evaluated before procedure application. since the function p calls itself with
;; no base case specified, the program goes into infinite recursion and does not terminate.
;;
;; on the other hand, with an interpreter using normal-order evaluation, the call to the function
;; test would proceed with all formal parameters being substituted with the given arguments 0 and (p)
;; without, it is important to note, their being evaluated. next, the if-expression performs a
;; check on the predicate (= x 0), which returns #t since x in this case is 0. the program
;; terminates by ultimately returning 0, leaving y, which has been substituted with (p) unevaluated.

;; 1.6
;; alyssa p. hacker doesn't see why if needs to be provided as a special form. "why can't i just
;; define it as an ordinary procedure in terms of cond?" she asks. alyssa's friend eva lu ator
;; claims this can indeed be done, and she defines a new version of if:
;;
;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))
;;
;; eva demonstrates the programs for alyssa:
;; (new-if (= 2 3) 0 5)
;; (new-if (= 1 1) 0 5)
;;
;; delighted, alyssa uses new-if to rewrite the square-root program:
;;
;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x) x)))
;;
;; what happens when alyssa attemps to use this to compute square roots? explain.
;;
;; alyssa's attempt would result in an infinite loop. since new-if is a general form, it follows
;; applicative-order evaluation, that is, when evoked, the involved subexpressions must be evaluated
;; before being applied as arguments to its body -- evaluation before application. in its use in the
;; sqrt-iter procedure, the following are evaluated before new-if's cond expression is applied:
;;
;; - (good-enough? guess x)
;; - guess
;; - (sqrt-iter (improve guess x) x)))
;;
;; sqrt-iter is a recursive procedure that depends on the termination of its call to new-if to cease.
;; however, due to new-if's being a general form and not a special form, sqrt-iter is evoked again
;; and again, as is new-if. this goes on indefinitely.

;; 1.7
;; the good-enough? test used in computing square roots will not be very effective for finding the
;; square roots of very small numbers. also, in real computers, arithmetic operations are almost
;; always performed with limited precision. this makes our test inadequate for very large numbers.
;; explain these statements, with examples showing how the test fails for small and large numbers.
;; an alternative strategy for implementing good-enough? is to watch how guess changes from one
;; iteration to the next and to stop when the change is a very small fraction of the guess. design
;; a square-root procedure that uses this kind of end test. does this work better for small and
;; large numbers?

;; iter 1:
;; (define (sqrt x)
;;   (define INITIAL_GUESS 1.0)
;;   (define EPSILON 0.00000001)
;;
;;   (define (good-enough? guess x)
;;     (< (abs (- (square guess) x)) EPSILON))
;;
;;   (define (improve guess x)
;;     (/ (+ (/ x guess) guess) 2))
;;
;;   (define (sqrt-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (sqrt-iter (improve guess x) x)))
;;
;;   (sqrt-iter INITIAL_GUESS x))
;;
;; (sqrt 0.00000000004) => inaccurate result
;; (sqrt 4444444444444444444444444444444444444444) => calculation does not complete

(+ 5 5 5)

;; iter 2:
(define (sqrt-v2 x)
  (define INITIAL_GUESS 1.0)
  (define EPSILON 0.00000001)

  (define (good-enough? guess x)
    (< (abs (- guess (improve guess x))) EPSILON))

  (define (improve guess x)
    (/ (+ (/ x guess) guess) 2))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter INITIAL_GUESS x))

(sqrt-v2 9)
(sqrt-v2 (+ 100 37))
(sqrt-v2 (+ (sqrt-v2 2) (sqrt-v2 3)))
(square (sqrt-v2 1000))
(sqrt-v2 0.00000000004)
(sqrt-v2 4444444444444444444444444444444444444444)

;; 1.8
;; newton's method for cube roots is based on the fact that if y is an approximation to the cube root
;; of x, then a better approximation is given by the value:
;;
;; (/ (+ (/ x (square y)) (* 2 y)) 3)
;;
;; use this formula to implement a cube-root procedure analogous to the square-root procedure. (in
;; section 1.3.4 we will see how to implement newton's method in general as an abstraction of these
;; square-root and cube-root procedures.)

;; iter 1:
;; (define (cube-root x)
;;   (define INITIAL_GUESS 1.0)
;;   (define EPSILON 0.00000001)
;;
;;   (define (good-enough? guess x)
;;     (< (abs (- guess (improve guess x))) EPSILON))
;;
;;   (define (improve guess x)
;;     (/ (+ (/ x (square guess)) (* 2 guess)) 3))
;;
;;   (define (cube-root-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (cube-root-iter (improve guess x) x)))
;;
;;   (cube-root-iter INITIAL_GUESS x))

;; iter 2:
;; include use of lexical scoping
(define (cube-root x)
  (define initial-guess 1.0)
  (define epsilon 0.00001)

  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) epsilon))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (cube-root-iter guess)
    (if (good-enough? guess)
      guess
      (cube-root-iter (improve guess))))

  (cube-root-iter initial-guess))

(cube-root 27)
(cube-root 19683)
(cube-root 21484952)
