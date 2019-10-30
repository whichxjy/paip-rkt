#lang racket

;; A grammar for a trivial subset of English.
(define *simple-grammar*
  (make-parameter
   '((sentence -> (noun-phrase verb-phrase))
     (noun-phrase -> (article noun))
     (verb-phrase -> (verb noun-phrase))
     (article -> the a)
     (noun -> man ball woman table)
     (verb -> hit took saw liked))))

;; The grammar used by generate.
(define *grammar* *simple-grammar*)

;; ===================================================================

;; The left hand side of a rule.
(define (rule-lhs rule)
  (first rule))

;; The right hand side of a rule.
(define (rule-rhs rule)
  (rest (rest rule)))

;; Return a list of the possible rewrites for this category.
(define (rewrites category)
  (let ([rule (assoc category (*grammar*))])
    (if rule
        (rule-rhs rule)
        null)))

;; ===================================================================

;; Generate a random sentence or phrase
(define (generate phrase)
  (if (list? phrase)
      (mappend generate phrase)
      (let ([choices (rewrites phrase)])
        (if (null? choices)
            (list phrase)
            (generate (random-choose choices))))))

;; ===================================================================

;; Generate a random sentence or phrase, with a complete parse tree.
(define (generate-tree phrase)
  (if (list? phrase)
      (map generate-tree phrase)
      (let ([choices (rewrites phrase)])
        (if (null? choices)
            (list phrase)
            (cons phrase
                  (generate-tree (random-choose choices)))))))

;; ===================================================================

;; Generate a list of all possible expansions of this phrase.
(define (generate-all phrase)
  (cond
    [(null? phrase)
     (list null)]
    [(list? phrase)
     (combine-all (generate-all (first phrase))
                  (generate-all (rest phrase)))]
    [(not (null? (rewrites phrase)))
     (mappend generate-all (rewrites phrase))]
    [else
     (list (list phrase))]))

;; Return a list of lists formed by appending a y to an x.
;; E.g., (combine-all '((a) (b)) '((1) (2)))
;; -> ((a 1) (a 2) (b 1) (b 2)).
(define (combine-all xlist ylist)
  (for*/list ([x xlist]
              [y ylist])
    (append x y)))

;; ===================================================================

;; Apply func to each element of list and append the results.
(define (mappend func lst)
  (apply append (map func lst)))

;; Choose an element from a list at random.
(define (random-choose lst)
  (list-ref lst (random (length lst))))