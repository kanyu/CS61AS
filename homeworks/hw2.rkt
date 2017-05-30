#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute

(define (try-sub cur-word old-word new-word)
  (if (equal? cur-word old-word)
    new-word
    cur-word))
(define (substitute sent old-word new-word)
  (if (empty? sent)
    '()
    (se (try-sub (first sent) old-word new-word) (substitute (bf sent) old-word new-word))))

; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: #<procedure>

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns: 25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns: 25

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|
Ex.
> (define (g) (lambda (n) (+ n 2)))
> ((g) 1)
3

Number of arguments g has: 0

Type of value returned by g: #<procedure>

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5
#|
1. f1
2. (f2)
3. (f3 3)
4. ((f4))
5. (((f5)) 3)
|#

(define f1 5)
; 1. (define f1 5) -> returns: 5

(define (f2) (+ 2 2))
; 2. (define (f2) (+ 2 2)) -> returns: 4

(define (f3 n) (+ n 3))
; 3. (define (f3 n) (+ n 3)) -> returns: 6

(define (f4) (lambda () (+ 1 1)))
; 4. (define (f4) (lambda () (+ 1 1))) -> returns: 2

(define (f5) (lambda () ( lambda (n) (+ n 5))))
; 5. (define (f5) (lambda () ( lambda (n) (+ n 5)))) -> returns: 8



; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9 

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3 

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  (lambda (new-wd) (equal? new-wd wd)))

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
 (if (> a b)
   1
   (* (term a)
      (product term (next a) next b))))

(define (factorial x)
   (product identity 1 inc x))

(define (estimate-pi)
  (define (pi-estimater n)
   (define (term x)
      (/ (* 4.0 (square x))
         (- (* 4.0 (square x)) 1)))
   (* 2.0 (product term 1 inc n)))
  (pi-estimater 1000))

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (my-accumulate combiner null-value term (next a) next b))))

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  (my-accumulate + 0 term a next b))

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  (my-accumulate * 1 term a next b))


; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  (if (> a b)
    null-value
    (if (pred a)
      (combiner (term a)
                (filtered-accumulate combiner null-value term (next a) next b pred))
      (filtered-accumulate combiner null-value term (next a) next b pred))))

(define (sum-sq-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  (define (my-rel-prime? i)
    (rel-prime? i n))
  (filtered-accumulate * 1 identity 1 inc (- n 1 ) my-rel-prime?))

; SICP 1.40 - Define cubic

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; SICP 1.41 - Define double

(define (double proc)
  (lambda (x) (proc (proc x))))

; SICP 1.43 - Define repeated

(define (my-repeated proc n)
  (if (= n 1)
    proc
    (compose proc (my-repeated proc (- n 1)))))

; Exercise 9 - Define my-every

(define (my-every proc sent)
  (if (empty? sent)
    '()
    (se (proc (first sent)) (my-every proc (bf sent)))))

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns: '(pp uu rr pp ll ee)

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns: '(781 5 7676 909 2424)

(keep even? '(781 5 76 909 24))
-> returns: '(76 24)

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns: 'ooeee

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns: ""

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns: Invalid arguments to MEMBER?:  purple aeiou

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns: '(purple)
|#