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
                    visible
                    children
                    child-count
                    vertex-count
                    fill-args
                    stroke-args
                    translation) (values  #f ; shape-struct
                                          #f ; finalized
                                          #t ; visible
                                          '() ; children
                                          0 ; child-count
                                          0 ; vertex-count
                                          #f; fill-args
                                          #f; stroke-args
                                          (cons 0 0) ;translation
                                        ))
    (define/public (begin-shape [kind 'default])
      (set! finalized #f)
      (set! shape-struct (new-shape kind)))
    (define/public (end-shape [closed? #t])
      (set! shape-struct (finish-shape shape-struct 'end-shape closed?))
      (set! finalized #t))
    (define/public (vertex x y)
      (if (not finalized)
          (begin
            (set! vertex-count (add1 vertex-count))
            (set-shape-rev-points! shape-struct
                                   (cons (cons x y) (shape-rev-points shape-struct))))
          (error 'vertex "vertex can't be added to a finalized shape.")))
    (define/public (get-vertex-count)
      vertex-count)
    (define/public (draw [x 0] [y 0])
      (when (and finalized visible)
        (define tx (+ (car translation) x))
        (define ty (+ (cdr translation) y))
        (draw-shape shape-struct tx ty fill-args stroke-args)
        (for ([c children])
          (send c draw x y))))
    (define/public (visible?)
      visible)
    (define/public (set-visible v)
      (set! visible v))
    (define/public (get-child-count)
      child-count)
    (define/public (add-child child)
      (set! child-count (add1 child-count))
      (set! children (cons child children)))
    (define/public (get-child index)
      (if (>= index child-count)
          (error 'get-child "index is greater or equal than the number of children: index: ~a, child-count: ~a" index child-count)
          (list-ref children index)))
    (define/public (remove-children)
      (set! child-count 0)
      (set! children '()))
    (define/public (set-fill . args)
      (set! fill-args args))
    (define/public (set-stroke . args)
      (set! stroke-args args))
    (define/public (translate x y)
      (set! translation (cons x y)))
    (super-new)))

(define (shape-create)
  (new shape%))
