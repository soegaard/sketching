#lang sketching

(define (draw)
  (define n frame-count)
  (background 125)
  (stroke "red")
  (stroke-weight 5)
  (stroke-cap    'round)
  (stroke-join   'miter)
  (fill "green")
  (line 150 0  150  height)
  (line 0  150 width 150 )
  (ellipse 300 300 50 100)
  (ellipse-mode 'corners)
  (ellipse 150 150 50 100)
  (fill "blue")
  (stroke "white")
  (arc 50 50 100 100 0 3.14)
  (line 0 0 width height)
  (ellipse-mode 'center)
  (stroke-weight 1)
  (circle (/ width 2) (/ height 2) 30)
  (quad 300 100 300 200 400 200 400 100)
  (rect-mode 'center)
  (rect 300 50 20 40 5)
  (triangle 10 10 20 20 0 50)
  )

(define value 0)


(define (setup)
  (size 600 400)
  (pixel-density 2)
  (displayln (list       width       height))
  (displayln (list pixel-width pixel-height)))

(define (mouse-pressed)
  (case mouse-button
    [(left)  (fill 255)]
    [(right) (fill 0)]))

(define (mouse-dragged)
  (set! value (+ value 5))
  (when (> value 255)
    (set! value 0)))


(frame-rate 30)
