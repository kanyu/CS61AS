#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.

#|
> (define x (list 1 2 3))
> (define y (list 4 5 6))
> (append x y)
'(1 2 3 4 5 6)
> (cons x y)
'((1 2 3) 4 5 6)
> (list x y)
'((1 2 3) (4 5 6))
|#

; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b. Define total-weight.
(define (branch-weight branch)
   (if (pair? (branch-structure branch))
       (total-weight (branch-structure branch))
       (branch-structure branch)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
; c. Define balanced?

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
  
(define (balanced? b-mobile)
  (error "Not yet implemented")) 

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))


;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (cond ((null? d-l) null)
        ((not (pair? d-l)) (square d-l))
        (else (cons (square-tree (car d-l)) (square-tree (cdr d-l))))))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (fn tree))
        (else (cons (tree-map fn (car tree)) (tree-map fn (cdr tree))))))

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
	          (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (j) (dot-product j v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


;Exercise 6 - Give the property that op should satisfy:

#|
> (fold-right / 1 (list 1 2 3))
1 1/2
> (fold-left / 1 (list 1 2 3))
1/6
> (fold-right list null (list 1 2 3))
(1 (2 (3 ())))
> (fold-left list null (list 1 2 3))
(((() 1) 2) 3)

Your property here
- commutative property
|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond ((and (null? l1) (null? l2))
          #t)
        ((or (null? l1) (null? l2))
          #f)
        ((and (symbol? l1) (symbol? l2))
          (eq? l1 l2))
        ((and (list? l1) (list? l2))
          (and (my-equal? (car l1) (car l2))
               (my-equal? (cdr l1) (cdr l2))))
        (else
          #f)))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (a) (cons (car s) a)) rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
