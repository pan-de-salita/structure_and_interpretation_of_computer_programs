#lang sicp

(define (f g) (g 2))
;; that is, (define f (lambda (g) (g 2)))

;; calling (f f) would lead to an error. consider:
;;
;; 1. apply f to f
;; (f f)
;; =>
;; 2. evaluate the subexpressions to expose the procedure bodies:
;; ((lambda (g) (g 2)) (lambda (g) (g 2)))
;; =>
;; 3. apply the first lambda expression (the operator) to the second (the operand):
;; (g 2)
;; =>
;; 4. evaluate g to expose (lambda (g) (g 2)) as the operator to the argument 2:
;; ((lambda (g) (g 2)) 2)
;;          |          |
;; (        g          2)
;; =>
;; 5. substitute the argument 2 as a "procedure" to the body of the lambda expression, resulting in:
;; (2 2) => thus producing an error because 2 is not a procedure that can be applied to arguments.
