#lang racket
(require test-engine/racket-tests)

;; 2.
;; Write a procedure squares that takes a sentence of numbers as its argument and returns a sentence
;; of the squares of the numbers:
;;
;; > (squares '(2 3 4 5))
;; 4 9 16 25

(define (square x) (* x x))

(define (squares a-list-of-numbers)
  (if (null? a-list-of-numbers)
    '()
    (cons (square (car a-list-of-numbers))
          (squares (cdr a-list-of-numbers)))))

(check-expect (squares '()) '())
(check-expect (squares '(1 2)) '(1 4))
(check-expect (squares '(2 3 4 5)) '(4 9 16 25))

;; 3.
;; Write a procedure switch that takes a sentence as its argument and returns a sentence in which
;; every instance of the words I or me is replaced by you, while every instance of you is replaced
;; by me except at the beginning of the sentence, where it's replaced by I. (Don't worry about
;; capitalization of letters.) Example:
;;
;; > (switch '(You told me that I should wake you up))
;; (i told you that you should wake me up)

(define (switch sentence)
  (define (switch-car car-sentence)
    (cond [(or (symbol=? car-sentence 'I)
               (symbol=? car-sentence 'Me))
           'You]
          [(symbol=? car-sentence 'You) 'I]
          [else car-sentence]))

  (define (switch-cdr cdr-sentence)
    (if (null? cdr-sentence)
        '()
        (cons (cond [(or (symbol=? (car cdr-sentence) 'I)
                         (symbol=? (car cdr-sentence) 'me))
                     'you]
                    [(symbol=? (car cdr-sentence) 'you) 'me]
                    [else (car cdr-sentence)])
              (switch-cdr (cdr cdr-sentence)))))

  (cons (switch-car (car sentence)) (switch-cdr (cdr sentence))))

(check-expect (switch '(You told me that I should wake you up)) '(I told you that you should wake me up))

;; 4.
;; write a predicate ordered? that takes a sentence of numbers as its argument and returns a true
;; value if the numbers are in ascending order, or a false value otherwise.

;; (define (ordered? a-list-of-numbers)
;;   (if (or (null? a-list-of-numbers) (null? (cdr a-list-of-numbers)))
;;     true
;;     (cond [(>= (car a-list-of-numbers) (cadr a-list-of-numbers)) false]
;;           [else (ordered? (cdr a-list-of-numbers))])))

(define (ordered? list-of-numbers)
  (or (or (null? list-of-numbers)
          (null? (cdr list-of-numbers)))
      (and (<= (car list-of-numbers)
               (cadr list-of-numbers))
           (ordered? (cdr list-of-numbers)))))

(check-expect (ordered? '()) true)
(check-expect (ordered? '(0)) true)
(check-expect (ordered? '(0 1 2 3)) true)
(check-expect (ordered? '(1 0 2 3)) false)

;; 5.
;; write a procedure ends-e that takes a sentence as its argument and returns a sentence containing
;; only those words of the arguments whose last letter is E:
;;
;; > (ends-e '(please put the salami above the blue elephant))
;; (please the above the blue)

(define (ends-e sentence)
  (define (last-character word)
    (substring word (- (string-length word) 1)))

  (define (character=e? character)
    (string=? "e" character))

  (define (ends-e-iter sentence)
    (cond [(null? sentence) '()]
          [else (if (character=e? (last-character (symbol->string (car sentence))))
                    (cons (car sentence) (ends-e (cdr sentence)))
                    (ends-e (cdr sentence)))]))

  (ends-e-iter sentence))

(check-expect (ends-e '()) '())
(check-expect (ends-e '(a b c d e)) '(e))
(check-expect (ends-e '(a b c)) '())
(check-expect (ends-e '(please put the salami above the blue elephant)) '(please the above the blue))

(test)
