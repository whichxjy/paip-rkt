#lang racket

(require "GPS2.rkt")

;; General Problem Solver for Maze
(define (Maze-GPS state *ops* goals)
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
    ;; Is x something that is (start) or (executing ...)?
    (define (action? x)
      (or (eq? x '(start))
          (executing? x)))
    ;; Solve problem
    (filter action? (achieve-all (cons '(start) state) goals null))))

;; ==============================================================

;; Make an operator to move between two places
(define (make-maze-op here there)
  (make-op
   `(move from ,here to ,there)
   `((at ,here))
   `((at ,there))
   `((at ,here))))

;; Make maze ops in both directions
(define (make-maze-ops pair)
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

;; ==============================================================

;; Find the Y in (executing (move from X to Y))
(define (destination action)
  (fifth (second action)))

;; Search a maze for a path from start to end.
(define (find-path start ops end)
  (let ([results (Maze-GPS `((at ,start)) ops `((at ,end)))])
    (cond
      [(null? results) null]
      [else
       (cons start
             (map destination
                  (remove '(start) results)))])))

;; ==============================================================

(module+ test
  (require rackunit)

  (define *maze-ops*
    (make-parameter
     (flatten
      (map make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14)
                   (9 8) (8 7) (7 12) (12 13)
                   (12 11) (11 6) (11 16) (16 17)
                   (17 22) (21 22) (22 23) (23 18)
                   (23 24) (24 19) (19 20) (20 15)
                   (15 10) (10 5) (20 25))))))
  
  (let* ([state '((at 1))]
         [*ops* (*maze-ops*)]
         [goals '((at 25))]
         [result (GPS state *ops* goals)])
    (check-equal? result
                  '((start)
                    (executing (move from 1 to 2))
                    (executing (move from 2 to 3))
                    (executing (move from 3 to 4))
                    (executing (move from 4 to 9))
                    (executing (move from 9 to 8))
                    (executing (move from 8 to 7))
                    (executing (move from 7 to 12))
                    (executing (move from 12 to 11))
                    (executing (move from 11 to 16))
                    (executing (move from 16 to 17))
                    (executing (move from 17 to 22))
                    (executing (move from 22 to 23))
                    (executing (move from 23 to 24))
                    (executing (move from 24 to 19))
                    (executing (move from 19 to 20))
                    (executing (move from 20 to 25))
                    (at 25))))

  (let* ([start 1]
         [ops (*maze-ops*)]
         [end 25]
         [path (find-path start ops end)])
    (check-equal? path
                  '(1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25))))