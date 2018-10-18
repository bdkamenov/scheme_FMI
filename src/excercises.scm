#lang racket/base

(require rackunit rackunit/text-ui)

; simple sum function

(define (add x y) (+ x y))

(define add-tests
  (test-suite
   "Tests for add"

   (check = (add 1 2) 3)
   (check = (add 1 1) 2)
   (check = (add 0 1) 1)
   (check = (add 4 -2) 2)
   (check = (add -4 2) -2)))

(run-tests add-tests)

; simple odd and even bool funcs

(define (odd? x) (= (modulo x 2) 1))

(define (even? x) (= (modulo x 2) 0))

(define even?-odd?-tests
  (test-suite
   "Tests for even? and odd?"

   (check-true (even? 0))
   (check-true (even? 2))
   (check-false (even? 1))
   (check-false (even? 3))

   (check-false (odd? 0))
   (check-false (odd? 2))
   (check-true (odd? 1))
   (check-true (odd? 3))))

(run-tests even?-odd?-tests)

; sign function

(define (signum x)
  (cond [(= x 0) 0]
        [(< x 0) -1]
        [(> x 0) 1])
        )

(define signum-tests
  (test-suite
   "Tests for signum"

   (check = (signum 0) 0)
   (check = (signum 1) 1)
   (check = (signum 3) 1)
   (check = (signum -1) -1)
   (check = (signum -3) -1)))

(run-tests signum-tests)

; factorial function

(define (factorial x)
  (if (= x 1)
      1
      [* x (factorial (- x 1))]
      ))

(define factorial-tests
  (test-suite
   "Tests for factorial"

   (check = (factorial 0) 1)
   (check = (factorial 1) 1)
   (check = (factorial 2) 2)
   (check = (factorial 3) 6)
   (check = (factorial 4) 24)
   (check = (factorial 5) 120)
   (check = (factorial 6) 720)
   (check = (factorial 7) 5040)))

(run-tests factorial-tests)

; sum interval [a:b]

(define (sum from to)
  (cond [(= from to) 0]
        [(< from to) (+ from (sum (+ from 1) to))]
        ))