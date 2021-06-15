#lang sketching

;; Multiple constructors

;; A class can have multiple constructors that assign the fields in different ways.
;; Sometimes it's beneficial to specify every aspect of an object's data by
;; assigning parameters to the fields, but other times it might be appropriate to define only one or a few.


;;;
;;; Spot
;;; 

(class Spot (x y radius)
  (cond
    [(not (or x y radius))
     ; no arguments were passed, default values are used
     (:= radius 40)
     (:= x      (* width 0.25))
     (:= y      (* height 0.5))]
    [else
     ; the initialization arguments were passed
     ; (and used automatically)
     (void)])

  (define/public (display)    
    (ellipse x y (* radius 2) (* radius 2))))

;;;
;;; Application
;;;

(define sp1 #f)
(define sp2 #f)

(define (setup)
  (size 640 360)
  (background 204)
  ; (fill 255)
  (no-loop)
  ;; Run the constructor without parameters
  (:= sp1 (make-object Spot))
  (displayln sp1)
  ;; Run the constructor with three parameters
  (:= sp2 (make-object Spot (* width 0.5) (* height 0.5) 120)))

(define (draw)
  (sp1.display)
  (sp2.display))



