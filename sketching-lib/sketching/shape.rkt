#lang racket/base

(provide shape%
         shape-create)

(require racket/class
         sketching/graphics)

; shape% class to represent storable shapes.
(define shape%
  (class object%
    (define-values (shape-struct
                    finalized
                    visible) (values #f
                                     #f
                                     #t))
    (define/public (vertex x y)
      (if (not finalized)
        (set-shape-rev-points! shape-struct
                               (cons (cons x y) (shape-rev-points shape-struct)))
        (error 'vertex "vertex can't be added to a finalized shape."))
      (displayln (shape-rev-points shape-struct)))
    (define/public (begin-shape [kind 'default])
      (set! finalized #f)
      (set! shape-struct (new-shape kind)))
    (define/public (end-shape [closed? #t])
      (set! shape-struct (finish-shape shape-struct 'end-shape closed?))
      (set! finalized #t))
    (define/public (draw [x 0] [y 0])
      (when (and finalized visible)
        (draw-shape shape-struct x y)))
    (define/public (visible?)
      visible)
    (define/public (set-visible v)
      (set! visible v))
    (super-new)))

(define (shape-create)
  (new shape%))
