#lang sketching
;; Multiple constructors

;; Original text:
;;   A class can have multiple constructors that assign the fields in different ways.
;;   Sometimes it's beneficial to specify every aspect of an object's data by
;;   assigning values to the fields, but other times it might be appropriate to define only one or a few.

;; Sketching:
;;    We use init fields with default values instead of explicit constructors.

;;;
;;; Spot
;;; 

(class Spot Object
  (init-field
   [x      (* width 0.25)]
   [y      (* height 0.5)]
   [radius 40])  
  ; "Constructor"  
  (super-new)
  ; Methods
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
  (no-loop)
  ;; Run the constructor without parameters
  (:= sp1 (make-object Spot))
  ;; Run the constructor with three parameters
  (:= sp2 (make-object Spot (* width 0.5) (* height 0.5) 120)))

(define (draw)
  (sp1.display)
  (sp2.display))



