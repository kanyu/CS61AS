#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
    (error "Division by 0") 
    (mul-interval x 
                  (make-interval (/ 1 (upper-bound y))
                                 (/ 1 (lower-bound y))))))


;SICP 2.12 - Define make-center-percent and percent
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c pct) 
 (let ((width (* c (/ pct 100.0)))) 
   (make-interval (- c width) (+ c width)))) 
  
(define (percent i) 
 (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0)) 
       (width (/ (- (upper-bound i) (lower-bound i)) 2.0))) 
   (* (/ width center) 100))) 

; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (if (null? (cdr lst))
    lst
    (last-pair (cdr lst))))

; SICP 2.20 - Define same-parity
(define (same-parity first . rest) 
 (define (same-parity-iter source dist remainder-val) 
   (if (null? source) 
       dist 
       (same-parity-iter (cdr source) 
                         (if (= (remainder (car source) 2) remainder-val) 
                             (append dist (list (car source))) 
                             dist) 
                         remainder-val))) 
 (same-parity-iter rest (list first) (remainder first 2))) 

; SICP 2.22 - Write your explanation in the comment block:

#|
1. He conses the square then the item, leaving the new items in the front
2. He is consing a list (answer)
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (cond ((null? lst) '())
        ((list? (car lst)) (cons (substitute (car lst) old new) (substitute (cdr lst) old new)))
        ((equal? (car lst) old) (cons new (substitute (cdr lst) old new)))
        (else (cons (car lst) (substitute (cdr lst) old new)))))

; Exercise 3 - Define my-substitute2

(define (substitute2 lst old new)
  (error "Not yet implemented"))
