#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/MouseFunctions/MouseFunctions.pde
; Mouse Functions.

; Click on the box and drag it across the screen. 

(define bx         0) ; center of box
(define by         0)
(define box-size  75) ; box radius

(define locked?   #f) 
(define x-offset   0) ; mouse position relative to center of box
(define y-offset   0)

(define (setup)
  (size 640 360)
  (:= bx (/ width 2.))
  (:= by (/ width 2.))
  (rect-mode 'radius))

(define (in-the-box? x y)
  (and (< (- bx box-size) x (+ bx box-size))
       (< (- by box-size) y (+ by box-size))))
       
(define (draw)
  (background 0)
  (cond
    [(and (in-the-box? mouse-x mouse-y) locked?)
     (stroke 255)
     (fill   255)]    
    [(and (in-the-box? mouse-x mouse-y) (not locked?))
     (stroke 255)
     (fill   153)]
    [else
     (stroke 153)
     (fill   153)])
  
  (rect bx by box-size box-size))

(define (on-mouse-pressed)
  (:= locked?  (in-the-box? mouse-x mouse-y))
  (:= x-offset (- mouse-x bx))
  (:= y-offset (- mouse-y by)))

(define (on-mouse-dragged)
  (when locked?
    (:= bx (- mouse-x x-offset))
    (:= by (- mouse-y y-offset))))

(define (on-mouse-released)
  (:= locked? #f))
