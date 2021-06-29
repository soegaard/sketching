#lang sketching

(class Circle Object
  (init [start-x 10] [start-y 20] [start-r 30]) ; initialization variables, needed for construction
  (field [x 0] [y 1] [r 2])                     ; field names and default expressions (can refer to init vars)
  ;; "Constructor"
  (super-new)    ; super class initialization
  (:= x (* 2 start-x))
  (:= y (* 2 start-y))
  (:= r (* 2 start-r))
  ;; Methods
  (define/public (grow [dr 1])
    (:= r (+ r dr))))

(define c (new Circle))
(list c.x c.y c.r)   ; field names are visible on the outside
; c.start-x          ; initialization variables are not
(c.grow)             ; c
(c.grow 10)          ; c
    
(define c2 (new Circle [start-x 1000]))


(define (setup)
  (size 640 360)
  (no-loop)
  (no-gui))

(define (draw)
  (void))


         
