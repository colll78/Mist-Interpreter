#lang racket


(require "ast.rkt")
(require "skeleton-interpreter.rkt")
(require rackunit)
(require profile)
(provide (all-defined-out))


;; Read-write cell
(define (rw-cell x)
  (define (dispatch m)
    (if (empty? m)
        x
        (rw-cell (first m))
        ))dispatch)

;; Read-only cell.
(define (ro-cell x)
  (define (dispatch m)
    (if (empty? m)
        x
        (ro-cell x)
    ))dispatch)

;; Interperse
(define (intersperse l v)
  (define (intersperse-iter accum l)
    (cond ((or (empty? l) (empty? (rest l))) (accum l))
        (else (intersperse-iter
               (lambda (remain) (accum (cons (first l) (cons v remain))))
               (rest l)
               ))))
  (intersperse-iter (lambda (x) x) l))

;; Generic find
(define (find pred l)
  (let loop ((index 0) (accum l))
    (if (empty? accum)
        #f
        (if (empty? (rest accum))
            (if (pred index (first accum))
                (cons index (first accum))
                #f
                )
            (if (pred index (first accum))
                (cons index (first accum))
                (loop (+ index 1) (rest accum))
                )
            )
        )))

;; Member using find
(define (member x l)
  (if (find (lambda (h t) (equal? t x)) l)
      #t
      #f
      ))

;; index-of using find
(define (index-of l x)
  (let ((ele (find (lambda (h t) (equal? t x)) l)))
    (if ele
        (car ele)
        #f
        )))

;; uncurry, tail-recursive
(define (uncurry f)
    (lambda (x)
    (define (uncurry-aux func inp-list)  
       (if (empty? (rest inp-list))
           (func (first inp-list))
          (uncurry-aux (func (first inp-list)) (rest inp-list))))
    (if (empty? x)
      (f)
      (uncurry-aux f x)
    )))             

;; Parse a quoted AST
(define (parse-ast node)
  (define (make-define-func node)
    ;; Utilizing syntax transformation to convert define-func's like:
    ;; (define (foo x y) (+ x y))
    ;;          into
    ;; (define foo (lambda (x y) (+ x y)))
    (let ((lambda-transformation (cons 'lambda (cons (rest (first (rest node))) (rest (rest node))))))
        (r:define
         (r:variable (first (define-head node)))
         (make-lambda lambda-transformation)
          )))
  (define (make-define-basic node)
    (r:define (parse-ast (define-head node)) (parse-ast (first (define-body node)))))
  (define (make-lambda node)
    (let ((params (map parse-ast (lambda-params node))) (args (map parse-ast (lambda-body node))))
      (r:lambda params args)
      ))
  (define (make-apply node)
    (r:apply (parse-ast (apply-func node)) (map parse-ast (apply-args node))))
  (define (make-number node)
    (r:number node))
  (define (make-variable node)
    (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
