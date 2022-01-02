#lang errortrace racket
(require "interpreter-utility.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;;---- Reference for formalisms ----
;;
;; e1 ⇓E v1
;(define* v1 (d:eval-exp* env e1))
;; E' <- E + [x = v1]
;(define* env2 (environ-push* env y v1)
;; e2 ⇓E' v2
;(define* v2 (d:eval-exp* env2 e2))

;; Evaluation of expressions using a mutable environment.
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
  (define (do-apply mem env exp)
    ;; ef ⇓E (Ef, λx.tb)
    (define ef (d:eval-term mem env (d:apply-func exp)))
    (define ef-mem (eff-state ef))
    (define ef-v (eff-result ef))
    ;; ea ⇓E va
    (define ea (d:eval-term ef-mem env (d:apply-arg1 exp)))
    (define ea-mem (eff-state ea))
    (define ea-v (eff-result ea))
    ;; Eb ← Ef + [x := va]
    (define Eb (environ-push ea-mem (d:closure-env ef-v) (d:lambda-param1 (d:closure-decl ef-v)) ea-v))
    (define Eb-mem (eff-state Eb))
    (define Eb-v (eff-result Eb))
    (define to-eval (d:lambda-body (d:closure-decl ef-v)))
    (d:eval-term Eb-mem Eb-v to-eval)
    )
  (cond ((d:value? exp) (eff mem exp))
        ((d:variable? exp)(eff mem (environ-get mem env exp)))
        ((d:lambda? exp) (eff mem (d:closure env exp)))
        ((d:apply? exp) (do-apply mem env exp))
        )
  )

;; Evaluation of terms using a mutable environment.
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  (define (do-def mem env term)
    ;; we need to evaluate the body of the define
    (define to-eval (d:define-body term))
    ;;e ⇓E v
    (define expression-ret (d:eval-exp mem env to-eval))
    ;; get the resulting value and memory state 
    (define v (eff-result expression-ret))
    (define mem-state (eff-state expression-ret))
    ;; E <- [x := v]
    ;; update E to reflect the variable definition, and update heap.
    (define new-mem-state (environ-put mem-state env (d:define-var term) v))
    ;; finally, the result evaluates down to void, but with state E <- [x := v]
    (eff new-mem-state (d:void)))
  (define (do-seq mem env term)
    ;; evaluate the seequence recursively, updating the heap to the resulting state of the
    ;; last evaluation at each step.
    (d:eval-term (eff-state (d:eval-term mem env (d:seq-fst term))) env (d:seq-snd term)))
  (cond
    ((d:define? term) (do-def mem env term))
    ((d:seq? term) (do-seq mem env term))
    (else (d:eval-exp mem env term))))  






#|
 The Language λD has side effects, which is sort of uncharacteristic of functional programming languages as languages with side effects are not truly pure functional programming languages.
This means that in λD, the order in which sub-expressions are evaluated impacts the result of the evaluation, this is due to the nature of the variable binding method used. 


For instance, the following code behaves differently in Racket than Language λD. 

(define (f x)
    (lambda (y) x))


(define g (f 10))
(g 20)
(define h (f 30))
(h g)

|#

