#lang racket/base

(provide succeed fail == fresh conde)

(define (var? term) (symbol? term))
(define (constant? term) (not (var? term)))

(define (extend subst key val)
  `((,key . ,val) . ,subst))

(define (follow term env)
  (cond [var? (let ([next (assq term env)])
                (cond
                  [next (follow (cdr next) env)]
                  [else term]))]
        [else term]))

(define (unify term1 term2 env)
  (let ([terminal1 (follow term1 env)]
        [terminal2 (follow term2 env)])
    (cond
      [(eq? terminal1 terminal2) env]
      [(var? terminal1) (extend env terminal1 terminal2)]
      [(var? terminal2) (unify terminal2 terminal1 env)]
      [(and (pair? terminal1) (pair? terminal2))
       (let ((env-first-unified (unify (car terminal1) (car terminal2) env)))
         (unify (cdr terminal1) (cdr terminal2) env-first-unified))]
      [else #f])))

(define (unif-node ))

(define (conj goal1 goal2) ())
