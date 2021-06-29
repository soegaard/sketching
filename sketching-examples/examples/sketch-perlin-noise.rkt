#lang sketching
(require
 racket/math
 metapict/pt-vec
 metapict/structs)

; Function to linearly interpolate between a0 and a1
; Weight w should be in the range [0.0, 1.0]
(define (interpolate a0 a1 w)
  (+ (* (- a1 a0) w) a0)
  #;(+ (* (- a1 a0) (* (- 3. (* w 2.)) w w) a0))                  ; smoothstep
  #;(+ (* (- a1 a0) (* (+ (* w (- (* w 6.) 15.)) 10) w w w)) a0)) ; smootherstep

; Create random direction vector
;; (define (random-gradient ix iy)
;;   (define r
;;     (* 2920.
;;        (sin (+ (* ix 21942.) (* iy 171324.) 8912.))
;;        (cos (+ (* ix 23157.     iy 217832.) 9758.))))
;;   (vec (cos r) (sin r)))

(define (random-gradient ix iy)
  (define r (random (* 2 pi)))
  (vec (cos r) (sin r)))

;; Computes the dot product of the distance and gradient vectors.
(define (dot-grid-gradient ix iy x y)
  ;; Get gradient from integer coordinates
  (define gradient (random-gradient ix iy))
  ;; Compute the distance vector
  (define dx (- x ix))
  (define dy (- y iy))
  ;; Compute the dot-product
  (+ (* dx (vec-x gradient))  (* dy (vec-y gradient))))

;; Compute Perlin noise at coordinates x, y
(define (perlin x y)
  ;; Determine grid cell coordinates
  (define x0 (int x))
  (define x1 (+ x0 1))
  (define y0 (int y))
  (define y1 (+ y0 1))

  ;; Determine interpolation weights
  ;; Could also use higher order polynomial/s-curve here
  (define sx (- x x0))
  (define sy (- x y0))
  
  ;; Interpolate between grid point gradients
  (define n0  (dot-grid-gradient x0 y0 x y))
  (define n1  (dot-grid-gradient x1 y0 x y))
  (define ix0 (interpolate n0 n1 sx))

  (define n2  (dot-grid-gradient x0 y1 x y))
  (define n3  (dot-grid-gradient x1 y1 x y))
  (define ix1 (interpolate n2 n3 sx))

  (interpolate ix0 ix1 sy))


(define (setup)
  (size 640 360)
  (no-loop))

(define (draw)
  (no-stroke)
  (define w 2)
  (define h 2)
  (for ([x (quotient width (* 4 w))])
    (for ([y (quotient height (* 4 h))])
      (fill (modulo (int (* 1. (perlin x y))) 128))
      (rect (* 4 x w) (* 4 y h) (* 4 w) (* 4 h)))))

      
      
