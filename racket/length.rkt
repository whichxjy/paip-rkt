#lang racket

(define (length1 lst)
  (for/fold ([len 0])
            ([element lst])
    (add1 len)))

(define (length2 lst)
  (let ([len 0])
    (map (lambda (element)
           (set! len (add1 len)))
         lst)
    len))

(define (length3 lst)
  (do ([len 0 (+ len 1)]
       [l lst (rest l)])
    ((null? l) len)))

(define (length4 lst)
  (count (lambda (x) #t) lst))

(define (length5 lst)
  (if (null? lst)
      0
      (add1 (length5 (rest lst)))))

(define (length6 lst)
  (length6-aux lst 0))

(define (length6-aux sublist len-so-far)
  (if (null? sublist)
      len-so-far
      (length6-aux (rest sublist) (add1 len-so-far))))

(define (length7 lst [len-so-far 0])
  (if (null? lst)
      len-so-far
      (length7 (rest lst) (add1 len-so-far))))

(define (length8 lst)
  (let loop ([lst lst]
             [len-so-far 0])
    (if (null? lst)
        len-so-far
        (loop (rest lst) (add1 len-so-far)))))

(module+ test
  (require rackunit)
  
  (check-equal? (length1 '()) 0)
  (check-equal? (length2 '()) 0)
  (check-equal? (length3 '()) 0)
  (check-equal? (length4 '()) 0)
  (check-equal? (length5 '()) 0)
  (check-equal? (length6 '()) 0)
  (check-equal? (length7 '()) 0)
  (check-equal? (length8 '()) 0)

  (check-equal? (length1 '(1 2 3 4)) 4)
  (check-equal? (length2 '(1 2 3 4)) 4)
  (check-equal? (length3 '(1 2 3 4)) 4)
  (check-equal? (length4 '(1 2 3 4)) 4)
  (check-equal? (length5 '(1 2 3 4)) 4)
  (check-equal? (length6 '(1 2 3 4)) 4)
  (check-equal? (length7 '(1 2 3 4)) 4)
  (check-equal? (length8 '(1 2 3 4)) 4))