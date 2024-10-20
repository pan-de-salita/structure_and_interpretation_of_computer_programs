#lang sicp

;; (Number -> Boolean)
;; (Number Number -> Number) Number
;; (Number Number -> Number) Number (Number Number -> Number) Number
;; -> Number
;; similar to the accumulate procedure defined in 1.32, but combines only those terms derived from
;; values in the range that satisfies a specified condition.

(define (filter-accumulate valid? combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (not (valid? a))
          (filter-accumulate valid? combiner null-value term (next a) next b)
          (combiner (term a)
                    (filter-accumulate valid? combiner null-value term (next a) next b)))))

;; Number -> Number
;; computes the square of a number.
(define (square n) (* n n))

;; Number -> Boolean
;; checks whether a number is a prime number.
(define (prime? n)
  (define (no-divisor? test)
    ;; (cond [(> test (/ n 2)) #t]
    ;;       [else (and (not (zero? (modulo n test)))
    ;;                  (no-divisor? (+ test 2)))])
    (or (> test (/ n 2))
        (and (not (zero? (modulo n test)))
             (no-divisor? (+ test 2)))))

  (cond [(= n 2) #t]
        [(or (< n 2) (even? n)) #f]
        [else (no-divisor? 3)]))

(eqv? (prime? 0) #f)
(eqv? (prime? 1) #f)
(eqv? (prime? 12) #f)
(eqv? (prime? 2) #t)
(eqv? (prime? 13) #t)

;; (Number -> Boolean)
;; (Number Number -> Number) Number
;; (Number Number -> Number) Number (Number Number -> Number) Number
;; -> Number
;; computes the sum of the squares of the prime numbers in the interval a to b.
(define (sum-of-primes-between a b)
  (filter-accumulate prime? + 0 square a (lambda (x) (+ x 1)) b))

(= (sum-of-primes-between 0 50) 10466)

;; computes the product of all the positive integers less than n that are relatively prime to n
;; (i.e., all positive integers i < n such that GCD(i, n) = 1).
(define (relative-prime-product n)
  (define (relatively-prime? i)
    (define (gcd=1? test)
      ;; (cond [(= test 1) #t]
      ;;       [else (if (not (and (zero? (modulo n test))
      ;;                           (zero? (modulo i test))))
      ;;               (gcd=1? (- test 1))
      ;;               #f)])
      (or (= test 1)
          (and (not (and (zero? (modulo n test))
                         (zero? (modulo i test))))
               (gcd=1? (- test 1)))))

    (gcd=1? (ceiling (/ n 2))))

  (filter-accumulate relatively-prime? * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) (- n 1)))

(= (relative-prime-product 5) 24)
(= (relative-prime-product 10) 189)
