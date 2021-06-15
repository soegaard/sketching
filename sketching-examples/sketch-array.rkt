#lang sketching

(define ys #f)
(define (y i) (vector-ref ys i))

(define (setup)
  (size 640 360)
  (set! ys (make-vector width 0.)))

(define (draw)
  (time
   (let ()
     (background 204)
     #;(vector-copy! ys 1  ys 0 (- width 1)) ; shift 1 right
     
     (for ([i (in-range (- width 1) 0 -1)])
       (:= ys i (y (- i 1))))
     
     (:= ys 0 mouse-y)
     
     (for ([i (in-range 1 width)])
       (line i (y i) (- i 1) (y (- i 1)))))))

      
    
