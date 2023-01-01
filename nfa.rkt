#lang racket
(require racket/list)

(provide nfa nfa-states)
(provide run-nfa concat-nfa union-nfa kleene-nfa cond-nfa copy-nfa)

;; NFA Functionality =======================================================================

;; An NFA is a
;; (make-nfa [List-of State] Transition State [List-of State])
;; On construction, cur represents the starting state.
(struct nfa (states transition cur accept-states))

;; A State is a Symbol

;; A Transition is a [State Char --> [List-of State]]
;; A Transition returns a list of all states which share a transition with q on input x.
;; If q has no transition on input x, the Transition must return the empty list.
;; For an epsilon transition, use #\nul or #\null 
(define SAMPLE-TRANS
  (λ(q x)
    (let
        ([state (λ(state input) (and (symbol=? state q) (char=? input x)))])
      (cond [(state 'q0 #\a) '(q0)]
            [(state 'q0 #\b) '(q0 q1)]
            [else '()]))))

;; run-nfa : NFA String --> Boolean
;; Runs the NFA on the input.
(define (run-nfa nfa str)
  (cond [(nfa-accept? nfa str) #t]
        [(zero? (string-length str))
         (ormap
          (λ(state) (run-nfa (nfa-step nfa state) str))
          (get-transitions nfa #\null))]
        [else (or (ormap
                   (λ(state) (run-nfa (nfa-step nfa state) str))
                   (get-transitions nfa #\null))
                  (ormap
                   (λ(state) (run-nfa (nfa-step nfa state) (substring str 1)))
                   (get-transitions nfa (string-ref str 0))))]))

;; NFA State --> NFA
(define (nfa-step n state)
  (nfa (nfa-states n)
       (nfa-transition n)
       state
       (nfa-accept-states n)))

;; NFA Char --> [List-of State]
(define (get-transitions nfa ch)
  ((nfa-transition nfa) (nfa-cur nfa) ch))

;; NFA String --> Boolean
;; Is the NFA currently in an accepting configuration?
(define (nfa-accept? nfa str)
  (and (member? (nfa-cur nfa) (nfa-accept-states nfa))
       (zero? (string-length str))))

;; Additional Features ====================================================

;; NFA NFA --> NFA
;; Assumes all unique names among states
(define (concat-nfa nfa1 nfa2)
  (nfa
   (append (nfa-states nfa1) (nfa-states nfa2))
   (λ(q x)
     (cond [(and (member? q (nfa-accept-states nfa1)) (char=? x #\null))
            (append (list (nfa-cur nfa2)) ((nfa-transition nfa1) q x))]
           [else (append ((nfa-transition nfa1) q x) ((nfa-transition nfa2) q x))]
           ;[(member? q (nfa-states nfa1)) ((nfa-transition nfa1) q x)]
           ;[(member? q (nfa-states nfa2)) ((nfa-transition nfa2) q x)]
           ;[else '(lol)]
           ))
   (nfa-cur nfa1)
   (nfa-accept-states nfa2)))

;; NFA NFA --> NFA
;; Assumes unique names
(define (union-nfa nfa1 nfa2)
  (let ([start (gensym 'start)]
        [end (gensym 'end)])
    (nfa
     (append (list start) (nfa-states nfa1) (nfa-states nfa2) (list end))
     (λ(q x)
       (cond [(and (symbol=? q start) (char=? x #\null))
              (list (nfa-cur nfa1) (nfa-cur nfa2))]
             [(and (member? q (nfa-accept-states nfa1)) (char=? x #\null))
              (cons end ((nfa-transition nfa1) q x))]
             [(and (member? q (nfa-accept-states nfa2)) (char=? x #\null))
              (cons end ((nfa-transition nfa2) q x))]
             [(member? q (nfa-states nfa1)) ((nfa-transition nfa1) q x)]
             [(member? q (nfa-states nfa2)) ((nfa-transition nfa2) q x)]
             [else '()]))
     start
     (list end))))

;; NFA --> NFA
(define (kleene-nfa n)
  (let ([start (gensym 'start)]
        [end (gensym 'end)])
    (nfa
     (append (list start) (nfa-states n) (list end))
     (λ(q x)
       (cond [(and (member? q (nfa-accept-states n)) (char=? x #\null))
              (cons end (cons (nfa-cur n) ((nfa-transition n) q x)))]
             [(and (symbol=? q start) (char=? x #\null)) (list (nfa-cur n) end)]
             [else ((nfa-transition n) q x)]))
     start
     (list end))))

;; NFA --> NFA
(define (cond-nfa n)
  (let ([state (gensym 'empty)])
    (union-nfa
     n
     (nfa
      (list state)
      (λ(q x) '())
      state
      (list state)))))

;; NFA --> NFA
;; Creates a copy of the NFA with new state names.
(define (copy-nfa n)
  (letrec ([new-states (map gensym (nfa-states n))]
           [new->old (make-hash (zip new-states (nfa-states n)))]
           [old->new (make-hash (zip (nfa-states n) new-states))])
    (nfa
     new-states
     (λ(q x) (map (λ(y) (hash-ref old->new y y)) ((nfa-transition n) (hash-ref new->old q q) x)))
     (hash-ref old->new (nfa-cur n))
     (map (λ(q) (hash-ref old->new q)) (nfa-accept-states n)))))


;; Utils ===================================================================

;; X [List-of X] --> Boolean
(define (member? item lst)
  (cond [(empty? lst) #f]
        [(cons? lst) (or (equal? (first lst) item))]))

; LoX Lox --> List of Pair of X
(define (zip l1 l2)
  (cond [(or (empty? l1) (empty? l2)) '()]
        [else (cons (cons (first l1) (first l2)) (zip (rest l1) (rest l2)))]))

;; (a|b)*b
(define N (nfa '(q0 q1) SAMPLE-TRANS 'q0 '(q1)))

;; (1|0)*0
(define M
  (nfa
   '(q2 q3)
   (λ(q x)
     (let
         ([state (λ(state input) (and (symbol=? state q) (char=? input x)))])
       (cond [(state 'q2 #\0) '(q2 q3)]
             [(state 'q2 #\1) '(q2)]
             [else '()])))
   'q2 '(q3)))

(define (print-transitions nfa alphabet)
  (map (λ(q) (map (λ(x) ((λ(y) (print (string-append (symbol->string q) ", " (string x) ":     ")) (print y) (println ""))
                         ((nfa-transition nfa) q x))) (cons #\null (string->list alphabet)))) (nfa-states nfa)))



(define NM (concat-nfa N M))
(define NorM (union-nfa N M))
(define N? (cond-nfa N))
(define N?M (concat-nfa N? M))
(define NM? (concat-nfa N (cond-nfa M)))
(define NM* (kleene-nfa (concat-nfa N M)))
(define N2 (copy-nfa N))