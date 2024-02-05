#lang racket/base

(require minikanren)
(require (rename-in racket/match (== match/==)))

(define (find-index-h var-name l n)
  (cond
    ((eq? l '()) #f)
    ((eq? (car l) var-name) n)
    (else (find-index-h var-name (cdr l) (+ n 1)))))

(define (find-index var-name l) (find-index-h var-name l 0))

(define (debruijinize term) (debruijinize-h term '()))

(define (debruijinize-h term indices)
  (let ([kind (car term)])
    (cond
      ((eq? kind 'type)  term)
      ((eq?  kind 'var)
       (list 'var (find-index (cadr term) indices)))
      ((or (eq? kind 'lambda) (eq? kind 'dep))
       (let* ([arg (cadr term)]
              [arg-type (caddr term)]
              [body (cadddr term)]
              [new-arg (debruijinize-h arg-type indices)]
              [new-body (debruijinize-h body (cons arg indices))])
         (list (car term) new-arg new-body))))))

(define zero 'z)

(define (succ n m)
  (== m `(s ,n)))

(define (decr n m) (succ m n))

(define (incr n) (succ n))

(define (expr-type expr-and-type expr type)
  (== expr-and-type `(,expr . ,type)))

(define (pattern-type expr n)
  (== expr `(type ,n)))

(define (pattern-lambda expr arg-type body)
  (== expr `(lambda ,arg-type ,body)))

(define (pattern-dep expr arg-type body)
  (== expr `(dep ,arg-type ,body)))

(define (pattern-app expr f arg)
  (== expr `(app ,f ,arg)))

(define (pattern-var expr var-index)
  (== expr `(var . ,var-index)))

(define (empty-env env)
  (== env '()))

(define (env-extended env index val extended-env)
  (conde ((== index zero)
          (== extended-env `(,val . ,env)))
         ((fresh (n subenv head tail-extended)
                 (decr index n)
                 (== env `(head . ,subenv))
                 (env-extended subenv n val tail-extended)
                 (== extended-env `(,head . ,tail-extended))))))

(define (env-val-at env index val)
  (env-extended env index val env))

(define (env-push old-env type new-env)
  (== `(,type . ,old-env) new-env))

(define (typecheck-higher expr type)
  (fresh (n m)
         (pattern-type expr n)
         (pattern-type type m)
         (incr n m)))

(define (typecheck-is-in-env env expr type)
  (fresh (var-index)
         (pattern-var expr var-index)
         (env-val-at env var-index type)))

(define (typecheck-lambda env expr type)
  (fresh (arg-type expr-body dep-body subenv)
         (pattern-lambda expr arg-type expr-body)
         (pattern-dep type arg-type dep-body)

         (env-push env arg-type subenv)
         (typecheck subenv expr-body dep-body)))

(define (typecheck-application env expr type)
  (fresh (app-abstraction app-arg arg-type prod x)
         (pattern-app expr app-abstraction app-arg)

         (expr-type app-arg arg-type)
         (typecheck env app-arg arg-type)
         (== prod `(dep ,arg-type ,type))
         (typecheck env app-abstraction prod)))

(define (typecheck-product env expr type)
  (fresh (arg-type dep-body subenv)
         (pattern-dep expr arg-type dep-body)
         (env-push env arg-type subenv)

         (typecheck subenv dep-body type)
         (typecheck env arg-type type)))

(define (typecheck env expr type)
  (conde
   (typecheck-higher expr type)
   (typecheck-is-in-env)
   (typecheck-lambda env expr type)
   (typecheck-application env expr type)
   (typecheck-product)))

(module+ test
  (require rackunit))

(module+ test
  (check-equal? (+ 2 2) 4))

(module+ main
  (require racket/cmdline)
  (define who (box "world"))
  (command-line
   #:program "my-program"
   #:once-each
   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
   #:args ()
   (printf "hello ~a~n" (unbox who))))
