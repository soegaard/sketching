#lang racket/base
(require racket/gui)

(define my-canvas%
  (class canvas%
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (define α 255)
      (define color (make-object color% 255 0 0 1.0))
      (define brush (new brush% [color color]))
      (send dc set-brush brush)
      (send dc set-text-foreground color)
      (send dc draw-text "Hello" 0 0)
      (send this resume-flush))
    (super-new)))

(define frame  (new frame% [label "sketch"]))
(define canvas (new my-canvas%
                    [parent     frame]
                    [min-width  640]
                    [min-height 350]))

(send frame show #t)

