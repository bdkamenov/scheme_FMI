#lang racket/base

(require rackunit)

(define eps 0.000001)

(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define // quotient)
(define % modulo)

; task 1

(define [fast-exp x n]
  (define [square x] (* x x))
  (cond [(equal? n 0) 1]
        [(equal? n 1) x]
        [#t (* x (fast-exp x (- n 1)))]
        ))

;(equal? (fast-exp 2 3) 8)
;(equal? (fast-exp -2 3) -8)
;(equal? (fast-exp 2.0 3) 8.0)
;(equal? (fast-exp 0 3) 0)
;(equal? (fast-exp 1 3) 1)


;task 2

(define [roots a b c]
  (define [disc a b c]
    (- (* b b) (* 4 a c)))
  (cond [(equal? (disc a b c) 0) 1]
        [(< (disc a b c) 0) 0]
        [#t 2]
        ))

;(equal? (roots 1 0 -4) 2)
;(equal? (roots 3 4 5) 0)
;(equal? (roots 1 4 -5) 2)
;(equal? (roots 1 4 4) 1)

; task 3

(define [fact n]
  (if (= n 1) 1
      (* n (fact (- n 1)))))

;(equal? (fact 1) 1)
;(equal? (fact 4) 24)
;(equal? (fact 5) 120)
;(equal? (fact 10) 3628800)

;task 4


(define (fib n)
  (define (helper i prev curr)
    (if (= i n)
        curr 
        (helper (++ i) curr (+ prev curr))))
  (helper 0 1 0)
  )

; task 5

(define (rev n)
  (define (do-rev res ost)
    (if (= ost 0) res
        (do-rev (+ (* res 10) (% ost 10)) (// ost 10))
          ))
  (if (< n 0) (- (do-rev 0 (- n)))
  (do-rev 0 n))
  )

; task 6

(define (palindrome? n)
  (if (= (rev n) n) #t
      #f))

; task 7

(define [devisors n]
  (define [helper sum prev n]
    (cond [(> prev (// n 2)) sum]
          [(= (% n prev) 0) (helper (+ sum prev) (+ prev 1) n)]
          [#t (helper sum (+ prev 1) n)]
          )
    )
  (helper 0 1 n)
)

; task 9

(define [prime? n]
  (define [helper dev n]
    (cond [(> dev (// n 2)) #t]
          [(= (% n dev) 0) #f]
          [#t (helper (+ dev 1) n)]
         )
    )
  (helper 2 n)
  )

; task 10

(define [incr? n]
  (define [helper prev curr rest]
    (cond [(and (< curr prev) (= rest 0)) #t]
          [(>= curr prev) #f]
          [#t (helper (% rest 10) (% (// rest 10) 10) (// rest 10))]
      )
    )
  (helper (% n 10) (% (// n 10) 10) n)
)

; task 11
; needs fix !!

(define [toBin n]
  (define [helper res val n]
    (cond [(= n 0) 0]
          [(= val 0) (helper (* res 10) (% n 2) (// n 2))]
          [(= val 1) (helper (+ (* res 10) 1) (% n 2) (// n 2))]
          )
    )
  (if (= n 0)
      0
     ; (helper 1 (% n 2) (// n 2))
      (helper 0 1 n)
      )
  )

(toBin 1)
(toBin 2)
(toBin 3)
(toBin 4)
(toBin 5)
(toBin 6)
(toBin 7)
(toBin 8)
(toBin 9)
(toBin 10)
  