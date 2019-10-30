#lang racket

(define (sentence)
  (append (noun-phrase) (verb-phrase)))

(define (noun-phrase)
  (append (article) (adj*) (noun) (pp*)))

(define (verb-phrase)
  (append (verb) (noun-phrase)))

(define (article)
  (one-of '(the a)))

(define (noun)
  (one-of '(man ball woman table)))

(define (verb)
  (one-of '(hit took saw liked)))

(define (adj*)
  (if (= (random 2) 0)
      null
      (append (adj) (adj*))))

(define (pp*)
  (if (random-choose '(t null))
      (append (pp) (pp*))
      null))

(define (adj)
  (one-of '(big little blue green adiabatic)))

(define (pp)
  (append (prep) (noun-phrase)))

(define (prep)
  (one-of '(to in by with on)))

;; ================================================

;; Pick one element of list, and make a list of it.
(define (one-of lst)
  (list (random-choose lst)))

;; Choose an element from a list at random.
(define (random-choose lst)
  (list-ref lst (random (length lst))))