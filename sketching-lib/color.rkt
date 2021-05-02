#lang racket/base
(require racket/draw
         racket/class
         racket/match
         racket/format
         "math.rkt")

(provide args->color
         color?
         color
         color-lerp         
         red green blue alpha)

(define (color? x)
  (and (object? x)
       (is-a? x color%)))

(define (args->color args who)
  ; todo: support color modes
  (match args
    [(list (? integer? rgb))        (make-object color% rgb rgb rgb)]
    [(list (? integer? rgb) a)      (make-object color% rgb rgb rgb (/ a 255))]
    [(list r g b)                   (make-object color% r    g    b)]
    [(list r g b a)                 (make-object color% r    g    b a)]
    [(list (? string? s))           (make-object color% s)]
    [(? (λ (x) (is-a? x color%)) c) c]
    [_                              (error who (~a "got: " args))]))

(define (color . args)
  (args->color args 'args))

(define (alpha col) (inexact->exact (* 255 (send col alpha))))
(define (red   col) (send col red))
(define (green col) (send col green))
(define (blue  col) (send col blue))

(define (exact-round x)
  (inexact->exact (round x)))

(define (color-lerp c1 c2 amt)
  (set! amt (min amt 1.))
  (set! amt (max amt 0.))
  (color (exact-round (lerp (red   c1) (red   c2) amt))
         (exact-round (lerp (green c1) (green c2) amt))
         (exact-round (lerp (blue  c1) (blue  c2) amt))))
