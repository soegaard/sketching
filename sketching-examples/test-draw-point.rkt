#lang sketching

(define (setup)
  (size 640 360)
  (stroke "red")
  (no-loop))


(define (draw)
  (for ([mode '(aligned smoothed unsmoothed)]
        [j    (in-naturals)])
    (smoothing mode)
    (for ([i 20])
      (stroke-weight (* i 0.5))
      (define x (+ 30 (* 25 i)))
      (define y (+ 20 (* j 25)))
      #;(line x y (+ x 0.0000000001) (+ y 0.0000000001))
      (point (+ 30 (* 25 i)) (+ 20 (* j 25))))))

    
  
