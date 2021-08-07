#lang info

(define deps '("draw-lib"
               "scribble-lib"
               "sketching"
               "base"))

(define build-deps '("racket-doc"
                     "sandbox-lib"))

(define scribblings '(("manual-sketching.scrbl" (multi-page) (language))))
