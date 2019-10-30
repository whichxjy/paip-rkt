#lang racket

(require racket/set racket/sequence)

;; Operation structure
(struct op (action preconds add-list del-list))

;; General Problem Solver: achieve all goals using *ops*.
(define (GPS *state* *ops* goals)
  (let ([*state* *state*]
        [*ops* *ops*])
    ;; A goal is achieved if it already holds, or if there is an
    ;; appropriate op for it that is applicable.
    (define (achieve goal)
      (or (set-member? *state* goal)
          (let ([appropriate-ops (for/list ([op (in-immutable-set *ops*)]
                                            #:when (appropriate-op? op goal))
                                   op)])
            (ormap apply-op appropriate-ops))))
    ;; An op is appropriate to a goal if it is in its add list.
    (define (appropriate-op? op goal)
      (set-member? (op-add-list op) goal))
    ;; Print a message and update *state* if op is applicable.
    (define (apply-op op)
      (if (sequence-andmap achieve (op-preconds op))
          (begin
            (displayln (list 'executing (op-action op)))
            (set-symmetric-difference! *state* (op-del-list op))
            (set-union! *state* (op-add-list op))
            #t)
          #f))
    ;; Solve problem
    (if (and (sequence-andmap achieve goals)
             (subset? goals *state*))
        'solved
        'unsolved)))

;; ==============================================================

(module+ test
  (require rackunit)
  
  (define *school-ops*
    (make-parameter
     (set
      (op 'drive-son-to-school
          (set 'son-at-home 'car-works)
          (set 'son-at-school)
          (set 'son-at-home))
      (op 'shop-installs-battery
          (set 'car-needs-battery 'shop-knows-problem 'shop-has-money)
          (set 'car-works)
          (set))
      (op 'tell-shop-problem
          (set 'in-communication-with-shop)
          (set 'shop-knows-problem)
          (set))
      (op 'telephone-shop
          (set 'know-phone-number)
          (set 'in-communication-with-shop)
          (set))
      (op 'look-up-number
          (set 'have-phone-book)
          (set 'know-phone-number)
          (set))
      (op 'give-shop-money
          (set 'have-money)
          (set 'shop-has-money)
          (set 'have-money)))))
  
  (let* ([*state* (mutable-set 'son-at-home 'car-needs-battery 'have-money 'have-phone-book)]
         [*ops* (*school-ops*)]
         [goals (set 'son-at-school)]
         [result (GPS *state* *ops* goals)])
    (displayln result)
    (check-equal? result 'solved))
  
  (let* ([*state* (mutable-set 'son-at-home 'car-needs-battery 'have-money)]
         [*ops* (*school-ops*)]
         [goals (set 'son-at-school)]
         [result (GPS *state* *ops* goals)])
    (displayln result)
    (check-equal? result 'unsolved))

  (let* ([*state* (mutable-set 'son-at-home 'car-works)]
         [*ops* (*school-ops*)]
         [goals (set 'son-at-school)]
         [result (GPS *state* *ops* goals)])
    (displayln result)
    (check-equal? result 'solved))

  (let* ([*state* (mutable-set 'son-at-home 'car-needs-battery 'have-money 'have-phone-book)]
         [*ops* (*school-ops*)]
         [goals (set 'have-money 'son-at-school)]
         [result (GPS *state* *ops* goals)])
    (displayln result)
    (check-equal? result 'unsolved)))