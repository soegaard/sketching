#lang sketching
(require metapict/pt-vec metapict/structs)

(define N 10)

(size 640 360)
(color-mode 'hsb 360 100 100)
(define ps (for/vector ([i N]) (pt  (random width) (random height)))) ; position
(define vs (for/vector ([i N]) (vec (random 5)     (random 5))))      ; velocity
(define as (for/vector ([i N]) (vec (random 1)     (random 1))))      ; acceleration
(define ts (for/vector ([i N]) (pt  (random width) (random height)))) ; target
(define cs (for/vector ([i N]) (color (remap i 0 N 0 360) 20 100)))    ; color

(define (setup)
  (background 0)
  (stroke-weight 4)
  (fill "red")
  #;(no-loop))

(define (step)
  (for ([p ps] [v vs] [a as] [t ts] [i (in-naturals)])
    (:= as i (limit-a (vec+ a (vec* 0.01 (pt- t p))) p v t))
    (:= vs i (limit (vec+ v a) p t))
    (:= ps i (pt+  p v))))

(define (draw)
  (step)
  ; draw targets
  (stroke-weight 20)
  (stroke "red")
  (for ([t ts])
    (point (pt-x t) (pt-y t)))
  ; draw points
  (stroke-weight 6)
  (for ([p ps] [c cs])
    (stroke c)
    (fill c)
    (point (pt-x p) (pt-y p))))


(define (limit v p t)
  (define d (dist p t)) ; distance to target
  (define m (if (> d 100) ; outside 100 pixels
                5        ; allow large maximum limit
                (remap d 0 100 0 5)))  ; map distance (0-100) into speed (10-0)
  (define n (norm v))
  (if (> n m)
      (vec* (/ m n) v) ; size m
      v))

(define (limit-a a p v t)
  (define d (dist p t))  
  (define n (norm a))
  (define m (* 0.1 (norm v)))
  (if (> n m) (vec* (/ m n) a) a))


(define (disturb)
  (for ([v vs] [i (in-naturals)])
    (:= as i (vec (random -1 1) (random -1 1)))
    (:= vs i (vec (random -200 200) (random -200 200)))))

(define (mouse-pressed)
  (disturb))



  
