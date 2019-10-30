#lang racket

(require "GPS2.rkt")

(module+ test
  (require rackunit)

  (define *banana-ops*
    (make-parameter
     (list
      (make-op 'climb-on-chair
               (list 'chair-at-middle-room 'at-middle-room 'on-floor)
               (list 'at-bananas 'on-chair)
               (list 'at-middle-room 'on-floor))
      (make-op 'push-chair-from-door-to-middle-room
               (list 'chair-at-door 'at-door)
               (list 'chair-at-middle-room 'at-middle-room)
               (list 'chair-at-door 'at-door))
      (make-op 'walk-from-door-to-middle-room
               (list 'at-door 'on-floor)
               (list 'at-middle-room)
               (list 'at-door))
      (make-op 'grasp-bananas
               (list 'at-bananas 'empty-handed)
               (list 'has-bananas)
               (list 'empty-handed))
      (make-op 'drop-ball
               (list 'has-ball)
               (list 'empty-handed)
               (list 'has-ball))
      (make-op 'eat-bananas
               (list 'has-bananas)
               (list 'empty-handed 'not-hungry)
               (list 'has-bananas 'hungry)))))

  (let* ([state (list 'at-door 'on-floor 'has-ball 'hungry 'chair-at-door)]
         [*ops* (*banana-ops*)]
         [goals (list 'not-hungry)]
         [result (GPS state *ops* goals)])
    (check-equal? result '((start)
                           (executing push-chair-from-door-to-middle-room)
                           (executing climb-on-chair)
                           (executing drop-ball)
                           (executing grasp-bananas)
                           (executing eat-bananas)))))