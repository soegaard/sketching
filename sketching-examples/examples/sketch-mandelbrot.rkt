#lang sketching
; Mandelbrot

(define xmin -2.5)
(define xmax  1.0)
(define ymin -1.0)
(define ymax  1.0)
(define dx     (- xmax xmin))
(define dy     (- ymax ymin))
(define aspect (/ dx dy))

(define max-iterations 50) ; what is reasonable here?

(define (setup)
  (define h 200)
  (size (int (* h aspect)) h)
  (color-mode 'hsb 360 100 100)
  (frame-rate 1)
  (no-loop))

(define (on-mouse-pressed)
  (displayln "pressed")
  (define x0 (remap mouse-x 0. width  xmin xmax))
  (define y0 (remap mouse-y 0. height ymin ymax))
  (define zoom 0.6)
  (define dx     (- xmax xmin))
  (define dy     (- ymax ymin))
  (:= xmin (- x0 (* zoom (/ dx 2.))))
  (:= xmax (+ x0 (* zoom (/ dx 2.))))
  (:= ymin (- y0 (* zoom (/ dy 2.))))
  (:= ymax (+ y0 (* zoom (/ dy 2.))))
  (loop)) ; make sure draw updates the image


(define (iterations x0 y0)
  (let loop ([iteration 0] [x 0.] [y 0.])
    (cond
      [(and (<= (+ (* x x) (* y y)) 4.)
            (< iteration max-iterations))
       (let ([xt (+ (* x x) (* -1. y y) x0)])
         (loop (+ iteration 1)
               xt (+ (* 2. x y) y0)))]
      [else
       iteration])))

(define (draw)
  (stroke-weight 1)
  (background 255)
  (smoothing 'unsmoothed)
  (no-loop)
  (time
  (for ([py (in-range 0 height)])
    (for ([px (in-range 0 width)])
      (define x0 (remap px 0. width  xmin xmax))
      (define y0 (remap py 0. height ymin ymax))
      (define n  (iterations x0 y0))
      (define c  (palette n))
      (stroke c)      
      (point px py)))))

(define (palette n)
  (define hue (int (remap n 0. max-iterations 0 360)))  
  (if (= n max-iterations)
      "black"
      (color hue 100 100 100)))
