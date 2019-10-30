#lang racket

;; Compute the mathematical dot product of two vectors.
(define (dot-product1 a b)
  (apply + (map * a b)))

;; Compute the mathematical dot product of two vectors.
(define (dot-product2 a b)
  (for/sum ([x a]
            [y b])
    (* x y)))

;; Compute the mathematical dot product of two vectors.
(define (dot-product3 a b)
  (if (or (null? a) (null? b))
      0
      (+ (* (first a) (first b))
         (dot-product3 (rest a) (rest b)))))

(module+ test
  (require rackunit)
  
  (check-equal? (dot-product1 '(10 20) '(3 4)) 110)
  (check-equal? (dot-product2 '(10 20) '(3 4)) 110)
  (check-equal? (dot-product3 '(10 20) '(3 4)) 110))