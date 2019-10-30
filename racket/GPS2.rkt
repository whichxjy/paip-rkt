#lang racket

;; Operation structure
(struct op (action preconds add-list del-list) #:mutable)

;; General Problem Solver: achieve all goals using *ops*.
(define (GPS state *ops* goals)
  (let ([*ops* *ops*])
    ;; Test if an element is equal to a member of a list.
    (define (member-equal? x xs)
      (ormap (lambda (element)
               (equal? x element))
             xs))
    ;; A goal is achieved if it already holds, or if there is an
    ;; appropriate op for it that is applicable.
    (define (achieve state goal goal-stack)
      (displayln (format "Goal ~a" goal))
      (cond
        [(member-equal? goal state) state]
        [(member-equal? goal goal-stack) null]
        [else
         (let ([appropriate-ops (filter (lambda (op)
                                          (appropriate-op? op goal))
                                        *ops*)])
           (ormap (lambda (op)
                    (apply-op state op goal goal-stack))
                  appropriate-ops))]))
    ;; Achieve each goal, and make sure they still hold at the end.
    (define (achieve-all state goals goal-stack)
      (let ([current-state state])
        (if (and (andmap (lambda (goal)
                           (set! current-state
                                 (achieve current-state goal goal-stack))
                           current-state)
                         goals)
                 (subset? goals current-state))
            current-state
            null)))
    ;; An op is appropriate to a goal if it is in its add list.
    (define (appropriate-op? op goal)
      (member-equal? goal (op-add-list op)))
    ;; Return a new, transformed state if op is applicable.
    (define (apply-op state op goal goal-stack)
      (displayln (format "Consider: ~a" (op-action op)))
      (let ([state2 (achieve-all state
                                 (op-preconds op)
                                 (cons goal goal-stack))])
        (cond
          [(null? state2) #f]
          [else
           ;; Return an updated state
           (displayln (format "Action: ~a" (op-action op)))
           (let ([deleted (filter (lambda (x)
                                    (not (member-equal? x (op-del-list op))))
                                  state2)])
             (append deleted (op-add-list op)))])))
    ;; Solve problem
    (filter pair? (achieve-all (cons '(start) state) goals null))))

;; ==============================================================

;; Is x of the form: (executing ...) ?
(define (executing? x)
  (start-with? x 'executing))

;; Is this a list whose first element is x?
(define (start-with? xs x)
  (and (pair? xs)
       (eq? (first xs) x)))

;; Make op conform to the (EXECUTING op) convention.
(define (op->executing-op op)
  (unless (ormap executing? (op-add-list op))
    (set-op-add-list! op
                      (cons (list 'executing (op-action op))
                            (op-add-list op))))
  op)

;; Make a new operator that obeys the (EXECUTING op) convention.
(define (make-op action preconds add-list del-list)
  (op->executing-op (op action preconds add-list del-list)))

;; ==============================================================

(module+ test
  (require rackunit)

  (define *school-ops*
    (make-parameter
     (list
      (make-op 'drive-son-to-school
               (list 'son-at-home 'car-works)
               (list 'son-at-school)
               (list 'son-at-home))
      (make-op 'shop-installs-battery
               (list 'car-needs-battery 'shop-knows-problem 'shop-has-money)
               (list 'car-works)
               (list))
      (make-op 'tell-shop-problem
               (list 'in-communication-with-shop)
               (list 'shop-knows-problem)
               (list))
      (make-op 'telephone-shop
               (list 'know-phone-number)
               (list 'in-communication-with-shop)
               (list))
      (make-op 'look-up-number
               (list 'have-phone-book)
               (list 'know-phone-number)
               (list))
      (make-op 'give-shop-money
               (list 'have-money)
               (list 'shop-has-money)
               (list 'have-money)))))

  (let* ([state (list 'son-at-home)]
         [*ops* (*school-ops*)]
         [goals (list 'son-at-home)]
         [result (GPS state *ops* goals)])
    (check-equal? result '((start))))

  (let* ([state (list 'son-at-home 'car-needs-battery 'have-money)]
         [*ops* (*school-ops*)]
         [goals (list 'son-at-school)]
         [result (GPS state *ops* goals)])
    (check-equal? result null))

  (let* ([state (list 'son-at-home 'car-needs-battery 'have-money 'have-phone-book)]
         [*ops* (*school-ops*)]
         [goals (list 'son-at-school 'have-money)]
         [result (GPS state *ops* goals)])
    (check-equal? result null))

  (let* ([state (list 'son-at-home 'car-needs-battery 'have-money 'have-phone-book)]
         [*ops* (*school-ops*)]
         [goals (list 'have-money 'son-at-school)]
         [result (GPS state *ops* goals)])
    (check-equal? result null))

  (let* ([state (list 'son-at-home 'car-needs-battery 'have-money 'have-phone-book)]
         [*ops* (*school-ops*)]
         [goals (list 'son-at-school)]
         [result (GPS state *ops* goals)])
    (check-equal? result
                  '((start)
                    (executing look-up-number)
                    (executing telephone-shop)
                    (executing tell-shop-problem)
                    (executing give-shop-money)
                    (executing shop-installs-battery)
                    (executing drive-son-to-school)))))