#lang racket

(require "nfa.rkt")

;; String String --> Boolean
(define (match? regex str)
  (run-nfa (compile-regex regex) str))

;; String String --> [List-of String]
(define (match regex str)
  '())

;; String --> NFA
(define (compile-regex regex)
  (reduce-nfas (parse-regex (string->list regex) '())))

;; [List-of Char] [List-of NFA] --> NFA
(define (parse-regex regex nfas)
  (cond [(empty? regex) nfas]
        [(cons? regex)
         (let ([token (first regex)])
           (cond [(char=? token #\*)
                  (parse-regex (rest regex) (cons (kleene-nfa (first nfas)) (rest nfas)))]
                 [(char=? token #\+)
                  (parse-regex (rest regex) (cons (plus-nfa (first nfas)) (rest nfas)))]
                 [(char=? token #\?)
                  (parse-regex (rest regex) (cons (cond-nfa (first nfas)) (rest nfas)))]
                 [(char=? token #\|)
                  (let* ([next-exp (first (split-first-exp (rest regex)))]
                        [left (second (split-first-exp (rest regex)))]
                        [next-nfa (compile-regex (list->string next-exp))])
                    (println next-exp)
                    (println left)
                    (parse-regex left (cons (union-nfa next-nfa (first nfas)) (rest nfas))))]
                 [(char=? token #\()
                  (let ([parexp (first (split-first-exp regex))]
                        [left (second (split-first-exp regex))])
                    (parse-regex
                     left
                     (cons (compile-regex (list->string parexp)) nfas)))]
                 ;; Every other non-functional character:
                 [else (parse-regex (rest regex) (cons (symbol-nfa token) nfas))]))]))


;; [List-of Char] --> [List-of [List-of Char]]
;; Splits the list into the first expression and the rest.
;; Trims the outermst layer of parentheses off the first expression, if necessary.
(define (split-first-exp loc)
  (cond [(char=? (first loc) #\() (parens-helper (rest loc) '(#\() 1)]
        [else (list (list (first loc)) (rest loc))]))

(define (trim-outer-parens loc)
  (if (and (char=? (first loc) #\()
           (char=? (last loc) #\)))
      (rest (reverse (rest (reverse loc))))
      loc))

;; [List-of Char] [List-of Char] Int --> [List-of [List-of Char]]
(define (parens-helper loc acc level)
  (cond [(or (empty? loc) (zero? level)) (list (trim-outer-parens acc) loc)]
        [(char=? (first loc) #\)) (parens-helper (rest loc) (append acc (list (first loc))) (sub1 level))]
        [(char=? (first loc) #\() (parens-helper (rest loc) (append acc (list (first loc))) (add1 level))]
        [else (parens-helper (rest loc) (append acc (list (first loc))) level)]))

;; [List-of NFA] --> NFA
;; Concatenates a list of NFAs together, preserving relative order.
(define (reduce-nfas nfas)
  (let ([q (gensym `q)])
    (foldl
     concat-nfa
     (nfa (list q) (λ(q x) '()) q (list q))
     nfas)))

;; Char --> NFA
(define (symbol-nfa char)
  (let ([start (gensym `start)]
        [end (gensym `end)])
    (nfa
     (list start end)
     (λ(q x) (if (and (symbol=? q start) (char=? x char)) (list end) '()))
     start
     (list end))))

;; NFA --> NFA
(define (plus-nfa n)
  (concat-nfa (copy-nfa n) (kleene-nfa n)))