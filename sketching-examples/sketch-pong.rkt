#lang sketching
(require metapict/pt-vec metapict/structs)

(define pos       (pt 0 0))
(define velocity  (vec 4 4))

(define (flipx v) (vec (- (vec-x v))    (vec-y v)))
(define (flipy v) (vec    (vec-x v)  (- (vec-y v))))


(define (show-ball b)
  (no-stroke)
  (fill "red")
  (circle (pt-x b) (pt-y b) 4))

(define (set-pos-ball! x y)
  (set! pos (pt x y)))

(define (update-ball!)
  (define v velocity)
  ; find new position p
  (define p (pt+ pos v))
  ; if out of bounds, reverse velocity
  (unless (< 0 (pt-x p) width)
    (:= velocity (flipx velocity)))
  (unless (< 0 (pt-y p) height)
    (:= velocity (flipy velocity)))
  ; store the new position
  (:= pos p))


(define (setup)
  (size 640 360))

(define (draw)
  (update-ball!)
  (show-ball pos))
