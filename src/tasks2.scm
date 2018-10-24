#lang racket/base

(define (accumulate op base from to map step)
  (if (> from to)
      base
      (accumulate op (op base (map from)) (step from) to map step)
      )
  )

;task 2

(define (fact n)
  (accumulate * 1 1 n (lambda(x) x) (lambda(x) (+ x 1)))
  )

(define (!! n)
  (accumulate * 1 (if(odd? n) 1 2) n (lambda(x) x) (lambda(x) (+ x 2)))
  )

;task 5

(define (devisors-sum n)
  (accumulate + 0 1 (- n 1) (lambda(x) (if (= (modulo n x) 0) x 0)) (lambda(x) (+ x 1)))
  )

;task 7

(define (count p? a b)
  (accumulate + 0 a b (lambda(x) (if (p? x) 1 0)) (lambda(x) (+ x 1)))
  )

;task 8

(define (all p? a b)
  (= (count p? a b) (+ (- b a) 1))
  )

(define (any p? a b)
  (= (count p? a b) 0)
  )

;task 9

(define (dev? x) (= (modulo 12 x) 0))

(define (prime n)
  (define (dev? x) (= (modulo n x) 0))
  (any dev? 2 n)
  )