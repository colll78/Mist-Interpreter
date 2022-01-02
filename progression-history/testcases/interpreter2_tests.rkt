#lang racket

(require rackunit)
(require "ast.rkt")
(require "interpreter2.rkt")
(define (cell-get c) (c (list)))
(define (cell-set c x) (c (list x)))


(define w1 (rw-cell 10))
(check-equal? 10 (cell-get w1))
(define w2 (cell-set w1 20))
(check-equal? 20 (cell-get w2))


(define r1 (ro-cell 10))
(check-equal? 10 (cell-get r1))
(define r2 (cell-set r1 20))
(check-equal? 10 (cell-get r2))


(check-equal? (list 1 0 2 0 3) (intersperse (list 1 2 3) 0))


(check-equal? (cons 0 10) (find (lambda (idx elem) #t) (list 10 20 30)))
(check-equal? #f (find (lambda (idx elem) #f) (list 10 20 30)))


(check-true (member 20 (list 10 20 30)))
(check-false (member 40 (list 10 20 30)))


(check-equal? 1 (index-of (list 10 20 30) 20))
(check-equal? #f (index-of (list 10 20 30) 40))


(define (f x y z w)
  (+ x y z w))
(define g (uncurry (curry f)))
(check-equal? 10 (g (list 1 2 3 4)))
(check-equal? 13 ((uncurry (lambda () 13)) (list)))
(check-equal? 13 ((uncurry (lambda (x) (+ x 3))) (list 10)))
(check-equal? 13 ((uncurry (lambda (x) (lambda (y) (+ x y)))) (list 10 3)))

(check-equal? (parse-ast 'x) (r:variable 'x))

(check-equal? (parse-ast '10) (r:number 10))

(check-equal?
  (parse-ast '(lambda (x) x))
  (r:lambda (list (r:variable 'x)) (list (r:variable 'x))))

(check-equal?
  (parse-ast '(define (f y) (+ y 10)))
  (r:define
    (r:variable 'f)
    (r:lambda
      (list (r:variable 'y))
      (list (r:apply (r:variable '+) (list (r:variable 'y) (r:number 10)))))))

(check-equal?
  (parse-ast '(define (f y) (+ y 10)))
  (r:define (r:variable 'f)
    (r:lambda (list (r:variable 'y))
      (list (r:apply (r:variable '+) (list (r:variable 'y) (r:number 10)))))))

(check-equal?
  (parse-ast '(define (f x y) (+ x y 10)))
  (r:define (r:variable 'f)
    (r:lambda (list (r:variable 'x) (r:variable 'y))
      (list (r:apply (r:variable '+) (list (r:variable 'x) (r:variable 'y) (r:number 10)))))))

(check-equal?
  (parse-ast '(define (f) (+ 2 3 4)))
  (r:define (r:variable 'f)
    (r:lambda '()
      (list (r:apply (r:variable '+) (list (r:number 2) (r:number 3) (r:number 4)))))))

(check-equal?
  (parse-ast '(define (f) 1))
  (r:define (r:variable 'f)
    (r:lambda '() (list (r:number 1)))))

(check-equal?
  (parse-ast '(define (f) (define x 3) x))
  (r:define (r:variable 'f)
    (r:lambda '()
      (list (r:define (r:variable 'x) (r:number 3)) (r:variable 'x)))))
