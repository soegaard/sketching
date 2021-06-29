#lang sketching

;;;
;;; Inheritance
;;;

;; A class can be defined using another class as a foundation.
;; In object-oriented programming terminology, one class can inherit fields and methods from another.
;; An object that inherits from another is called a subclass,
;; and the object it inherits from is called a superclass. A subclass extends the superclass.

(class Spin (x y speed angle)
  ; Constructor
  (:= angle 0.0)
  ; Methods  
  (define/public (update)
    (+= angle speed)))

(class SpinArm Spin ()
  (inherit-field x y speed angle) ; bring variables into scope
  ; Constructor
  ; (super-make-object x y speed) ; invoke super class initialization
  ; Methods
  (define/public (display)
    (stroke-weight 1)
    (stroke 0)
    (push-matrix)
    (translate x y)
    (+= angle speed)
    (rotate angle)
    (line 0 0 165 0)
    (pop-matrix)))

(class SpinSpots Spin (dim)
  (inherit-field x y speed angle)
  ; Constructor
  ; (super-make-object x y speed)
  ; Methods
  (define/public (display)
    (no-stroke)
    (push-matrix)
    (translate x y)
    (+= angle speed)
    (rotate angle)
    (ellipse (- (/ dim 2)) 0 dim dim)
    (ellipse (+ (/ dim 2)) 0 dim dim)
    (pop-matrix)))

(define spots #f)
(define arm   #f)

(define (setup)
  (size 640 360)
  (:= arm   (make-object SpinArm  (/ width 2) (/ height 2)  0.01 ))
  (:= spots (make-object SpinSpots
                         90.0                               ; new field
                         (/ width 2) (/ height 2) -0.02)))  ; super fields

(define (draw)
  (background 204)
  (arm.update)
  (arm.display)
  (spots.update)
  (spots.display))




