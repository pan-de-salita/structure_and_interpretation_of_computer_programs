#lang racket
(require test-engine/racket-tests)

(define (square x)
  (* x x))

(square (+ 2 3)) ;; => 25

;; formal parameters => the "x" in (square x)
;; actual argument expression => (* 2 3) in the body
;; actual argument value => 5 gotten from (+ 2 3)

(define (first-char-of-symbol symbol)
  (string->symbol (substring (symbol->string symbol) 0 1)))

(check-expect (first-char-of-symbol 'word) 'w)

(define (last-char-of-symbol symbol)
  (string->symbol
   (substring (symbol->string symbol)
              (- (string-length (symbol->string symbol)) 1))))

(check-expect (last-char-of-symbol 'word) 'd)

(define (but-last-of-symbol symbol)
  (string->symbol
   (substring (symbol->string symbol)
              0
              (- (string-length (symbol->string symbol)) 1))))

(check-expect (but-last-of-symbol 'word) 'wor)

(define (but-first-of-symbol symbol)
  (string->symbol (substring (symbol->string symbol) 1)))

(check-expect (but-first-of-symbol 'word) 'ord)

(define (vowel? symbol)
  (ormap (lambda (x) (string-suffix? (symbol->string symbol) x)) '("a" "e" "i" "o" "u")))

(check-expect (vowel? 'a) #t)
(check-expect (vowel? 'e) #t)
(check-expect (vowel? 'i) #t)
(check-expect (vowel? 'o) #t)
(check-expect (vowel? 'u) #t)
(check-expect (vowel? 'b) #f)

(define (consonant? symbol)
  (not (vowel? symbol)))

(check-expect (consonant? 'a) #f)
(check-expect (consonant? 'e) #f)
(check-expect (consonant? 'i) #f)
(check-expect (consonant? 'o) #f)
(check-expect (consonant? 'u) #f)
(check-expect (consonant? 'b) #t)

(define (plural word)
  (if (and (equal? (last-char-of-symbol word) 'y)
           (not (vowel? (last-char-of-symbol (but-last-of-symbol word)))))
      (string->symbol (string-append (symbol->string (but-last-of-symbol word)) "ies"))
      (string->symbol (string-append (symbol->string word) "s"))))

(check-expect (plural 'computer) 'computers)
(check-expect (plural 'book) 'books)
(check-expect (plural 'boy) 'boys)
(check-expect (plural 'fly) 'flies)
(check-expect (plural 'cry) 'cries)
(check-expect (plural 'activity) 'activities)


(define (symbol->list symbol)
  (build-list (string-length (symbol->string symbol))
              (lambda (idx) (substring (symbol->string symbol) idx (+ 1 idx)))))

(check-expect (symbol->list 'abc) '("a" "b" "c"))

(define (translate-to-pigl word)
  (define (translate-to-pigl-iter word-string)
    (if (vowel? (string->symbol (substring word-string 0 1)))
        (string-append word-string "ay")
        (translate-to-pigl-iter (string-append (substring word-string 1)
                                               (substring word-string 0 1)))))

  (string->symbol (translate-to-pigl-iter (symbol->string word))))

(check-expect (translate-to-pigl 'welcome) 'elcomeway)
(check-expect (translate-to-pigl 'hello) 'ellohay)
(check-expect (translate-to-pigl 'food) 'oodfay)
(check-expect (translate-to-pigl 'island) 'islanday)
(check-expect (translate-to-pigl 'eunoia) 'eunoiaay)
(check-expect (translate-to-pigl 'scheme) 'emeschay)

;; (define (pigl sentence)
;;   (if (null? sentence)
;;       '()
;;       (cons (translate-to-pigl (car sentence))
;;             (pigl (cdr sentence)))))

(define (pigl sentence)
  (map translate-to-pigl sentence))

(test)
