
#lang errortrace racket
(provide (all-defined-out))
(require "ast.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Substitution operation 
(define (s:subst exp var val)
    (cond ((s:number? exp) exp)
          ((s:variable? exp)
           (if (equal? exp var)
               val
               exp
               ))
          ((s:lambda? exp)
           (if (equal? (s:lambda-param1 exp) var)
               exp
               (s:lambda (s:lambda-params exp) (list (s:subst (s:lambda-body1 exp) var val)))
               ))
          ((s:apply? exp)
           (s:apply (s:subst (s:apply-func exp) var val)
                    (list (s:subst (s:apply-arg1 exp) var val))))
          )
  )

;; Evaluation of expressions using substitution
(define (s:eval subst exp)
  (cond ((s:value? exp) exp)
        ((s:apply? exp)
         (let ((e1 (s:eval subst (s:apply-func exp))))
           (let ((e2 (subst (s:lambda-body1 e1) (s:lambda-param1 e1) (s:eval subst (s:apply-arg1 exp)))))
               (s:eval subst e2)
               )
         ))))

;; Evaluation of expressions using environments. 
(define (e:eval env exp)
  (cond ((e:number? exp) exp)
        ((e:variable? exp) (hash-ref env exp))
        ((e:lambda? exp) (e:closure env exp))
        ((e:apply? exp)
         (let ((e1 (e:eval env (e:apply-func exp))))
           (let ((e2 (e:lambda-body1 (e:closure-decl e1))))
             (e:eval (hash-set (e:closure-env e1) (e:lambda-param1 (e:closure-decl e1)) (e:eval env (e:apply-arg1 exp))) e2)
             )))))

#|
 λ-Racket without environments is a better alternative than λ-Racket with environments in certain memory
critical systems where computational efficiency isn't a large concern. Take for instance, a small embedded
system which is only responsible for displaying some text (maybe weather / traffic information on the highway).
In such a device, implementing λ-Racket with environments might require an addition of hardware to the device
to support the memory overhead introduced by the hash-set. Likewise, it would be better to use λ-Racket without
environments for programs primarily consisting of small expressions, as while the time-complexity of this method
is O(N) where N is the size of the expression, the constants hidden by the asymptotic notation are relatively
small. Even thought the time-complexity of the function-call procedure in λ-Racket with environments is significantly
better than that of λ-Racket without environments, the constants hidden by the asymptotic notation are much larger.
Thus, for all values of N below a certain threshold, it would be better, in terms of both efficiency and memory, to
avoid the overhead introduced by the hash-set (and the hashing functions it performs behind the scenes) and instead
use the substitution method.

On the other hand, racket with environments should be used for most purposes, as memory is typically not an issue
for modern day computers, so it pays off to pay the overhead to introduce a hash-set thus improving the computational
efficiency of function calls for all but the most trivial of expressions (expressions with less than N sub-expressions
where N is some small value).  

 The existence of formal specification in the design of a software system is essential to the
creation of formal proofs. Such proofs provide insightful information that can aid in the system's
development process. Take for instance, the formal specifications for regular languages and
context-free languages, with which the chomsky language hierarchy was proven. Many software-engineers
have invested a great deal of time working on regular expressions to prevent cross-site scripting, but
the chomsky heirachy tells us that if a language is explicitly context-free (context-free and not regular)
then it is mathematically impossible for any regular expression to recognize it. If the deveoper of
https://regex101.com/r/rV7zK8/1 was familiar with the specifications for context-free gramamrs, they might
have saved themselves hundreds of hours of fruitless painstaking labor. 

 Additionally, when a system is designed using formal specifications, formal verification techniques can be
used to evaluate the design's correctness with respect to its specifications. This way, expensive large scale
refactoring can be avoided, as problems can be identified in the early stages of development. 
|#

