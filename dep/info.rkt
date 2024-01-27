#lang info
(define collection "dep")
(define deps '("base" "minikanren"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/dep.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(tonlage))
(define license '(Apache-2.0 OR MIT))
