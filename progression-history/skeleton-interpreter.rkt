#lang racket

;; An extremely basic interpreter.
;; This is the skeleton which the Mist Interpreter builds off of. 


(provide (all-defined-out))

(define ex1 (/ (/ 3 (/ 5 4)) (- (* 6 3) 12)))

;(define ex2
;  (list
;   (/ (/ 3 (/ 5 4)) (- (* 6 3) 12))
;   (/ (/ 3 (/ 5 4)) (- 18 12))
;   (/ (/ 3 (/ 5 4)) 6)
;   (/ (/ 3 1.25) 6)
;   (/ 2.4 6)
;   0.4
;   ))


(define ex2
  (list
   (/ (/ 3 (/ 5 4)) (- (* 6 3) 12))
   (/ (/ 3 5/4) (- (* 6 3) 12))
   (/ 12/5 (- (* 6 3) 12))
   (/ 12/5 (- 18 12))
   (/ 12/5 6)
   2/5
   ))

;(define ex3
;  (lambda (x y)
;    (< (* (+ y 13) (- 15 y)) (* (+ x y) x))))

(define (ex3 x y)
  (< (* (+ y 13) (- 15 y)) (* (+ x y) x)))

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))
;; Constructs a tree with a single node
(define (tree-leaf value) (list '() value '()))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value)
  (list (tree-left self)
        value
        (tree-right self)))

(define (tree-set-left self left)
  (list left
        (tree-value self)
        (tree-right self)))

(define (tree-set-right self right)
  (list (tree-left self)
        (tree-value self)
        right))

;; Function that inserts a value in a BST
(define (bst-insert self value)
  (cond ((null? self) (tree-leaf value))
        ((equal? value (tree-value self)) (tree-set-value self value))
        ((< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value)))
        (else (tree-set-right self (bst-insert (tree-right self) value)))))
     

;; lambda
(define (lambda? node)
  (and (not (null? node))
           (list? node)
           (>= (length node) 3)
           (eq? (car node) 'lambda)
           (list? (cadr node))
           (andmap symbol? (cadr node))
           ))

(define (lambda-params node) (cadr node))
(define (lambda-body node) (cddr node))

;; apply
(define (apply? l)
  (and (not (null? l))
           (list? l)
           ))
 
(define (apply-func node) (car node))
(define (apply-args node) (cdr node))

;; define
(define (define? node)
  (or (define-basic? node) (define-func? node)))

(define (define-basic? node)
  (and (list? node)
           (eq? (length node) 3)
           (eq? (car node) 'define)
           (symbol? (cadr node))
           ))

(define (define-func? node)
  (if (and (list? node)
           (>= (length node) 3)
           (eq? (car node) 'define)
           (list? (cadr node))
           (> (length (cadr node)) 0)
           (andmap symbol? (cadr node))
           )
      #t
      #f
  )
  )
