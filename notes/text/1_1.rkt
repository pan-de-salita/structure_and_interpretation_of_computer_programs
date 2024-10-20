#lang sicp

;;;;;; 1.1 ELEMENTS OF PROGRAMMING

;;;; 1.1.1 EXPRESSIONS

;; PRIMITIVE EXPRESSION
486

;; combination (compound expression)
;; general form: (<operator> <operand-0> ... <operand-n>)
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

;; the value of a combination is obtained by applying the procedure specified by the operator to
;; the arguments that are the values of the operands.

;; PREFIX NOTATION
;; - can accommodate procedures that may take an arbitrary number of arguments:
(+ 21 35 12 7)
(* 25 4 12)

;; can extend in a straightforward way to allow combinations to be nested (to have combinations
;; whose elements are themselves combinations):
(+ (* 3 5) (- 10 6))
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;; pretty-printing
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;; 1.1.2 NAMING AND THE ENVIRONMENT
(define size 2)
(* 5 size)

(define pi 3.14159)
(define radius 10)

(define (square x) (* x x))
(* pi (square radius))

(define circumference (* 2 pi radius))

circumference

;; NOTE: it should be clear that the possibility of associating values with symbols and later
;; retrieving them means that the interpreter must maintain some sort of memory that keeps track
;; of the name-object pairs. this memory is called the environment (more precisely the global
;; environment, since we will see later that a computation may involve a number of different
;; environments).

;; 1.1.3 EVALUATING COMBINATIONS

;; the interpreter:
;; to evaluate a combination, do the following:
;;
;; 1. evaluate the subexpressions of the combination.
;; 2. apply the procedure that is the value of the leftmost subexpression (the operator) to the
;;    arguments that are the values of the other subexpressions (the operands).

;; NOTE: the first step dictates that in order to accomplish the evaluation process for a
;; combination we must first perform the evaluation process on each element of the combination.
;; thus the evaluation is recursive in nature; that is, it includes, as one of its steps, the need
;; to invoke the rule itself.

(* (+ 2
      (* 4 6))
   (+ 3 5 7))
;; =>
(* (+ 2
      24)
   15)
;; =>
(* 26 15) ;; => 390

;; example of tree accumulation?

;; exceptions to the general evaluation rule: special forms, e.g.
;; - define

;; 1.1.4 COMPOUND PROCEDURES

;; PROCEDURE DEFINITIONS
;; a means of abstraction by which a compoound expression can be related to a name and referred to
;; as a unit

;; (define (square    x)         (*    x       x))
;;    |       |       |           |    |       |
;;    to    square something, multiply it by itself.

;; GENERAL FORM: (define (<name> <formal parameters>) <body>)
;; note the 2 operations:
;; - procedure creation
;; - associating a name to the created procedure

(square 21)
(square (+ 2 5))
(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;;;; 1.1.5 the substitution model for procedure application

;; to apply a compound procedure to arguments, evaluate the body of the procedure with each formal
;; parameter replaced by the corresponding argument.

(f 5)
;; =>
(sum-of-squares (+ 5 1) (* 5 2))
;; =>
(+ (square 6) (square 10))
;; =>
(+ (* 6 6) (* 10 10))
;; =>
(+ 36 100) ;; => 136

;; NOTE: the substitution model for procedure application does not reflect how procedure application
;; is carried out by the Scheme interpreter.

;; NOTE: applicative order vs normal order

;; NORMAL-ORDER EVALUATION
;; 'fully expand and then reduce'
(f 5)
;; =>
(sum-of-squares (+ 5 1) (* 5 2))
;; =>
(+ (square (+ 5 1)) (square (* 5 2)))
;; =>
(+ (* (+ 5 1) (+ 5 1))
   (* (* 5 2) (* 5 2)))
;; =>
(+ (* 6 6)
   (* 10 10))
;; =>
(+ 36 100) ;; => 136

;; APPLICATIVE-ORDER EVALUATION
;; 'evaluate the arguments and then apply'
;; in other words:
;; first evaluate the operator and operands
;; and then apply the resulting procedure to the resulting arguments

;; NOTE: used by the Lisp interpreter

(f 5)
;; =>
(sum-of-squares (+ 5 1) (* 5 2))
;; three subproblems:
;; - evaluate the operator to get the procedure to be applied, i.e. sum-of-square
;; - evaluate the operands to get the arguments, i.e. (+ 5 1) and (* 5 2)
;; =>
(+ (square 6) (square 10))
;; =>
(+ (* 6 6) (* 10 10))
;; =>
(+ 36 100) ;; => 136

;;;; 1.1.6 CONDITIONAL EXPRESSIONS AND PREDICATES

(define (abs-cond x)
  (cond ((< x 0) (- x))
        (else x)))

(abs-cond -1)
(abs-cond 0)
(abs-cond 1)

;; CONDITIONAL EXPRESSION
;; syntax: (cond (<p1> <e1>)
;;               (<p2> <e2>)
;;               ...
;;               (<pn> <en>))
;; each pair of of expressions is called a clause.
;; - <p> | predicate
;; - <e> | consequent expression

;; NOTE: if none of the <p>'s is found to be true, the value of the cond is undefined.

;; IF EXPRESSION
;; syntax: (if <p> <e> <a>)
;; <a> | alternative

(define (abs-if x)
  (if (< x 0)
      (- x)
    x))

(= (abs-if -1) (abs-cond -1))
(= (abs-if 0) (abs-cond 0))
(= (abs-if 1) (abs-cond 1))

;; NOTE: cond and if are special forms, and thus do not follow applicative evaluation order.
;; instead, it carries out evaluation by checking predicates one at a time, from top to bottom. when
;; it arrives at a predicate that evaluates to true, it evaluates its corressponding consquent
;; expression. the subsequent predicates and consequent expressions are left unevaluated.

;; LOGICAL COMPOSITION OPERATIONS
;; - (and <e1> ... <en>)
;; NOTE: if all <e>'s evaluate to true values, the value of the and expression is the value of the
;; last one.
;; e.g. (and (< 1 2) (+ 2 2)) => 4
;; - (or <e1> ... <en>)
;; - (not <e>)

;;;; 1.1.7 EXAMPLE: SQUARE ROOTS BY NEWTON'S METHOD

;; NOTE: difference between mathematical functions and computer procedures:
;; procedures must be effective.
;; in mathematics we are usually concerned with declarative (what is) descripitons, whereas in
;; computer science we are usually concerned with imperative (how to) descriptions.

;; given this description:
;; (sqrt x) = the y such that y >= 0 and y**2 == x.

;; own iter:
;; (define (sqrt x)
;;   (define INITIAL_GUESS 1)
;;
;;   (define (calc-sqrt x y)
;;     (if (> y (/ x 2))
;;       0
;;       (cond [(= (sqr y) x) y]
;;             [else (calc-sqrt x (+ y 1))])))
;;
;;   (calc-sqrt x INITIAL_GUESS))
;;
;; (sqrt 4) ;; => 2
;; (sqrt 5) ;; => 0

;; Newton's method of successive approximations

(define (sqrt x)
  (define INITIAL_GUESS 1.0)
  (define EPSILON 0.001)

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) EPSILON))

  (define (improve guess x)
    (define (average x y) (/ (+ x y) 2))
    (average guess (/ x guess)))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter INITIAL_GUESS x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))
(sqrt 0.000000000000000000000004)
(sqrt 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444)

;;;; 1.1.8 PROCEDURES AS BLACK-BOX ABSTRACTIONS

;; recursive procedure: a procedure defined in terms of itself

;; program as a cluster of procedures, mirroring the decompositin of the problem into subproblems

;; the point is not the arbitrary division of a program into smaller parts, but a design where each
;; procedure accomplishes an identifiable task that can be used as a module in defining other procedures.
;; procedure => procedural abstraction

(define (square-simple x) (* x x))

(define (square-complex x) (exp (double (log x))))
(define (double x) (+ x x))

;; using either square-simple of square-complex would not matter when writing good-enough?. anyy procedure
;; that computes the square is equally good.

;; LOCAL NAMES

;; a procedure definition binds its formal paramaters -- BOUND VARIABLE
;; the meaning of a procedure definition is unchanged if a bound variable is consistently renamed throughout
;; the definition

;; if a variable is not bound, we say it is free.

;; the set of expressions for which a binding defines a name is called the scope of that name. in a procedure
;; definition, the bound variables declared as the formal parameters of the procedure have the body of the
;; procedure as their scope

;; INTERNAL DEFINITIONS AND BLOCK STRUCTURE

(define (sqrt-block-structure x)
  (define INITIAL_GUESS 1.0)
  (define EPSILON 0.001)

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) EPSILON))

  (define (improve guess x)
    (define (average x y) (/ (+ x y) 2))
    (average guess (/ x guess)))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter INITIAL_GUESS x))

(define (sqrt-lexical-scoping x)
  (define INITIAL-GUESS 1.0)
  (define EPSILON 0.00001)

  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) EPSILON))

  ;; better test:
  ;; (define (good-enough? guess)
  ;;   (< (abs (- guess (improve guess))) EPSILON))

  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))

  (sqrt-iter INITIAL-GUESS))

(= (sqrt-block-structure 4) (sqrt-lexical-scoping 4))

;; recall:
;; the set of expressions for which a binding defines a name is called the scope of that name. in a procedure
;; definition, the bound variables declared as the formal parameters of the procedure have the body of the
;; procedure as their scope

;; LEXICAL SCOPING dictates that free variables in a procedure are taken to refer to bindings made by
;; enclosing procedure definitions; that is, they are looked up in the environment in which the procedure
;; was defined.
