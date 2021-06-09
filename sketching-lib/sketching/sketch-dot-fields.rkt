#lang sketching

(define v (vector 11 (vector 22 33)))
(list v.x v.y.x)

(struct Circle (x y r))

(define c (Circle 11 22 33))
(list c.x c.y c.r)

(v.ref 0)

(:= v.x 1)
(:= v.x 111)
(:= v.y.x 2)
(:= v.y.x 222)
(:= v.y.y 3)
(:= v.y.y 333)

(+= v.x 1000)
(-- v.x)

(define (setup)
  (void))

(define (draw)
  (line 0 0 width height)
  (void))
