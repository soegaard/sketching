#lang info

(define collection 'multi)

(define build-deps '("gui-doc"
                     "pict-doc"
                     "at-exp-lib"
                     "base"
                     "gui-lib"
                     "pict-lib"
                     "scribble-lib"
                     "sketching-lib"
                     "racket-doc"
                     "cairo-lib"))

(define update-implies '("sketching-lib"))

(define pkg-desc "documentation part of \"sketching\"")

(define pkg-authors '(soegaard))
