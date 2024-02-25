#lang racket/base

(define term-kind car)
(define (var? term) (= 'var (term-kind term)))
(define (lambda? term) (= 'lambda (term-kind term)))
(define (forall? term) (= 'forall (term-kind term)))
(define (app? term) (= 'app (term-kind term)))

(define (check-types expr-type env)
  (let ([expr (car expr-type)]
        [type (cdr expr-type)])
  (cond
    [(var? expr) ()]
    [(lambda? expr) ()]
    [(app? expr)] ())))

(module+ test
  (require rackunit))

(module+ test

  )

(module+ main
  (require racket/cmdline)
  (command-line
    #:args ()))
