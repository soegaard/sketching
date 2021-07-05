#lang sketching
; Original Processing example by hbarragan.
;   https://github.com/processing/processing-docs/tree/master/content/examples/Basics/Objects/CompositeObjects

;; /**
;;  * Composite Objects
;;  * 
;;  * An object can include several other objects. Creating such composite objects 
;;  * is a good way to use the principles of modularity and build higher levels of 
;;  * abstraction within a program.
;;  */

(class Ring Object
  (field [x #f] [y #f] [diameter #f] [on #f])
  ; x,y       x- and y-coordinate
  ; diameter  diameter of the ring
  ; on        turns the display on and off

  ;; Constructor
  ; values are used as is
  (super-new)
  
  ;; Methods
  (define/public (start xpos ypos)
    (:= x xpos)
    (:= y ypos)
    (:= on #t)
    (:= diameter (or diameter 1)))

  (define/public (grow)
    (when on
      (+= diameter 0.5)
      (when (> diameter (* 2 width))
        (:= diameter 0.0))))

  (define/public (display)
    (when on
      (no-fill)
      (stroke-weight 4)
      (stroke 155 153)
      (ellipse x y diameter diameter))))

(class Egg Object
  ; x, y     X-coordinate, y-coordinate
  ; tilt     Left and right angle offset
  ; angle    Used to define the tilt
  ; scalar   Height of the egg
  (init-field x y tilt scalar) ; passed to "constructor"

  ;; Constructor
  (super-new)
  (define angle 0.)    ; default value for floats in P are 0.0
  (/= scalar 100.)

  (define/public (wobble)
    (:= tilt (/ (cos angle) 8.))
    (+= angle 0.1))

  (define/public (display)
    (stroke "red")
    (fill 255)
    (push-matrix)
    (translate x y)
    (rotate tilt)
    (scale scalar)
    ;; beginShape();
    ;;   vertex(0, -100);
    ;;   bezierVertex(25, -100, 40, -65, 40, -40);
    ;;   bezierVertex(40, -15, 25, 0, 0, 0);
    ;;   bezierVertex(-25, 0, -40, -15, -40, -40);
    ;;   bezierVertex(-40, -65, -25, -100, 0, -100);
    ;; endShape();
    ;; TODO: Shapes aren't implemented yet, so let's
    ;;       approximate with an ellipse instead
    (ellipse-mode 'center)
    (ellipse 0 0 80 120)
    (pop-matrix)))


(class EggRing Object
  (init-field x y t sp)            ; arguments for the constructor
  (field [ovoid #f] [circle #f])   ; fields
  ;; Constructor
  (super-new)
  (:= circle (new Ring))
  (:= ovoid  (new Egg [x x] [y y] [tilt t] [scalar sp]))
  (circle.start x (- y (/ sp 2)))
  
  (define/public (transmit)
    (ovoid.wobble)
    (ovoid.display)
    (circle.grow)
    (circle.display)
    (unless circle.on
      (:= circle.on #t))))

;;;
;;;
;;;

(define er1 #f)
(define er2 #f)

(define (setup)
  ; (no-loop)
  (size 640 360)
  (frame-rate 60)
  (:= er1 (make-object EggRing (* width 0.45) (* height 0.5) 0.1  120))
  (:= er2 (make-object EggRing (* width 0.65) (* height 0.8) 0.05 180)))
  
(define (draw)
  (background 0)
  (er1.transmit)
  (er2.transmit))
