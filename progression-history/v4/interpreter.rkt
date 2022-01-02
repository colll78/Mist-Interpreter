#lang errortrace racket

(require "interpreter-utility.rkt")
(provide (all-defined-out))

(define/contract (env-put env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op
   (lambda (mem)
    (eff (environ-put mem env var val) (d:void))
    )))  

(define/contract (env-push env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op
   (lambda (mem)
    (environ-push mem env var val)
    )))

(define/contract (env-get env var)
  (-> handle? d:variable? eff-op?)
  (eff-op (lambda (mem)
    (eff mem (environ-get mem env var))
    )))

(define/contract (d:eval-exp env exp)
  (-> handle? d:expression? eff-op?)
   (define (do-apply func args)
     (match func
       ((d:apply (d:apply (d:variable 'if) (list antecedent)) (list then-consequent)) (do-if antecedent then-consequent args))
       ; above will match statements like the following: 
       ;#(struct:d:apply #(struct:d:apply #(struct:d:variable if) (#(struct:d:bool #f)) ) (#(struct:d:number 1)))#(struct:d:number 2)
       (_
     (do
         e-val <- ((d:eval-exp-impl) env args)
         e-fun <- ((d:eval-exp-impl) env func)
     (match e-fun
       ((d:builtin fun) (eff-pure (fun e-val)))
       ((d:closure env (d:lambda (list param) body))
        (do
            e-env <- (env-push env param e-val)
            e-result <- ((d:eval-term-impl) e-env body)
          (eff-pure e-result))))
       ))))
  (define (do-if antecedent then-consequent else-consequent)
       (do
          e-antecedent <- ((d:eval-exp-impl) env antecedent)
         (match e-antecedent
           ;antecedent evaluated to false then evaluate else-consequent
           ((d:bool #f) ((d:eval-exp-impl) env else-consequent))
           ;antecedent evaluated to anything that isn't #f then evaluate then-consequent
           (_ ((d:eval-exp-impl) env then-consequent))
           ))
    )
  (match exp
    ((d:variable _) (env-get env exp))
    ((? d:value? exp) (eff-pure exp))
    ((d:lambda _ _) (eff-pure (d:closure env exp)))
    ((d:apply func (list args)) (do-apply func args))
    )
  )

(define/contract (d:eval-term env term)
  (-> handle? d:term? eff-op?)
  (define (do-define lhs rhs)
    (do 
        e-rhs <- ((d:eval-exp-impl) env rhs)
    (env-put env lhs e-rhs)
    ))
  (define (do-sequence fst snd)
    (do
        e-fst <- ((d:eval-term-impl) env fst)
        e-snd <- ((d:eval-term-impl) env snd)
       (eff-pure e-snd))
    )
  (match term
    ((d:seq fst snd) (do-sequence fst snd))
    ((d:define lhs rhs) (do-define lhs rhs))
    (_ ((d:eval-exp-impl) env term)))
    )

;; Use this dynamic parameter in d:eval-term for improved testing (see Lecture 31)
(define d:eval-exp-impl (make-parameter d:eval-exp))
;; Use this dynamic parameter in d:eval-exp for improved testing (see Lecture 31)
(define d:eval-term-impl (make-parameter d:eval-term))

;; Parameter body *must* be a curried term already
(define/contract (break-lambda args body)
  (-> (listof d:variable?) d:term? d:lambda?)
  (if (empty? args)
        (d:lambda (list (d:variable '_)) body)
        (if (equal? (length args) 1)
            (d:lambda args body)
            (d:lambda (list (car args)) (break-lambda (cdr args) body))
        )
  ))



;; ef is a curried expression and args is a list of curried expressions
(define/contract (break-apply ef args)
  (-> d:expression? (listof d:expression?) d:expression?)
   (if (empty? args)
        (d:apply ef (list (d:void)))
        (if (equal? (length args) 1)
            (d:apply ef args)
            (break-apply (d:apply ef (list (car args))) (cdr args))
        )
  ))

;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-lambda-impl (make-parameter break-lambda))
;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-apply-impl (make-parameter break-apply))

(define/contract (d:curry term)
  (-> d:term? d:term?)
  (match term
    ((d:lambda (list args ...) body) ((break-lambda-impl) args (d:curry body)))
    ((d:apply ef (list args ...)) ((break-apply-impl) ef (map d:curry args)))
    ((d:define x (d:lambda args body)) (d:define x (d:curry (d:lambda args body))))
    ((d:seq fst snd) (d:seq (d:curry fst) (d:curry snd)))
    (_ term)
    )
  )
