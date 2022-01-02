#lang racket

#|
The barebones expression evaluator from which Mist expands upon. 
Additionally, included are various functions for constructing and interacting 
with promise lists (lazy evaluation), a lazy binary search tree, and infinite streams.
|#

(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))


(define p:empty (delay null))


(define (p:empty? p) (eq? (force p) null))

#|

Yes this is certainly possible. However, since we don't want to rely on
the user to delay the arguments himself, we're going to want to use
the (define-syntax ...) function. Indeed, this is a perfect showcase
of how powerful and easy syntax transformations are in racket.

(define-syntax p:cons
  (syntax-rules ()
    ((p:cons x l)
     (cons (delay x) l)
  )))

Then you can test that it works with the following line:
(car (p:cons (display "hi") (cons (delay 1) (delay 2))))

When evaluated, it will not display "hi".

This sort of behavior is not possible without using macros
either directly or via wrapper functions due to #lang racket's
eager evaluation procedure, unless of-course we require that
the user manually delay the argument like so:
(p:cons (delay x) l)

|#


(define (p:first l) (car (force l)))

(define (p:rest l) (cdr (force l)))


(define (p:append l1 l2)
  (if (p:empty? l1)
      l2
      (delay (cons (p:first l1) (p:append (p:rest l1) l2)))
  ))

  

;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

#|
Lazy evaluation binary search tree
|#

(define (bst->p:list self)
  (cond [(empty? self)p:empty]
    [else
     (p:append (bst->p:list (tree-left self))
      (delay (cons (tree-value self)
                  (bst->p:list (tree-right self)))))]))

#|

Here is a trivial example of a situation in which lazy evaluation outperforms eager evaluation.
The following function takes a promise list as input, and outputs a new promise list that consists of every other
element from the first list.

(define (every-other-ele plst) 
    (if (or (p:empty? plst) (p:empty? (p:rest plst)))
        p:empty
        (delay (cons (p:first (p:rest plst)) (every-other-ele (p:rest (p:rest plst)))))
    ))

If the input list was a standard list instead of the lazily evaluated promise list then each
and every element of the list would have to have been evaluated. This would have a huge impact on
performance, especially if the list elements were produced with function calls i.e:
(list (foo x y z) (bar z n m) (+ 3 2))

If the input list was generated as a promise list then only (bar z n m) would have to be evaluated.

Additionally, if the programmer only needs to find the first instance of element x from the output of
every-other-ele the above function will be much more efficient than an alternative version that does not
use lazy evaluation, since it does not have to evaluate every element in the list.  

|#

;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s)
  ((let fold-aux ((acc a) (strm s))
    (if (null? s)
        acc
        (thunk (cons acc (fold-aux (f (stream-get strm) acc) (stream-next strm))))
    ))))

;; Infinite stream skip n elements
(define (stream-skip n s)
  (let loop ((index 0) (srm s))
    (if (< index n)
        (if (eq? (stream-next s) null)
            (stream-next s)
            (loop (+ index 1) (stream-next srm))
        )
        srm
    )))


(struct r:bool (value)  #:transparent)

;; and that requires second parameter to be a list of the apply-args
(define (extended-and args)
  (let loop ((lst args))
    (if (empty? lst)
        #t
        (if (car lst)
            (if (null? (cdr lst))
                (car lst)
                (loop (cdr lst)))  
            #f
            ))))
        
  
  
(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) extended-and]
        [else #f]))

(define (r:eval-exp exp)
  ;(display exp)
 ; (display "\n")
  (cond
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a boolean value just return that boolean value
    [(r:bool? exp) (r:bool-value exp)]
    ; 4. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:apply? exp)
     (let ((v-name (r:variable-name (r:apply-func exp))))
     (cond ((eq? v-name 'and)
            ((r:eval-exp (r:apply-func exp)) (map r:eval-exp (r:apply-args exp))))
           ((eq? v-name '+)
            ((lambda (vals)  (foldl + 0 vals)) (map r:eval-exp (r:apply-args exp))))
           (else 
            ((r:eval-exp (r:apply-func exp))
             (r:eval-exp (first (r:apply-args exp)))
             (r:eval-exp (second (r:apply-args exp)))))))]
    [else (error "Unknown expression:" exp)]))
