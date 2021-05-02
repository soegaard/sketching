#lang racket/base

(provide
 ; abs
 ceil
 constrain
 dist
 ; floor
 lerp
 mag
 remap ; called map in P
 norm
 pow
 radians
 new-random
 sq
 sqr)

; abs
(define (ceil x) (ceiling x))

(define (constrain amt low high)
  (if (< amt low)
      low
      (if (> amt high)
          high
          amt)))

(define (dist x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))

      
(define (lerp start stop amt)
  (+ start (* amt (- stop start))))

(define (mag x y [z 0])
  (sqrt (+ (sqr x) (sqr y) (sqr z))))

(define (remap value start1 stop1 start2 stop2)
  (lerp start2 stop2 (/ (- value start1) (- start2 start1))))

(define (norm value start stop)
  (/ (- value start) (- stop start)))

(define (pow n e)
  (expt n e))


(define (sq  x) (* x x))
(define (sqr x) (* x x))


(define r/d (/ 3.141592653589793 180.))
(define (radians d)
  (* r/d d))

(define new-random
  (case-lambda
    [(high)     (* high (random))]
    [(low high) (+ low (* (- high low) (random)))]))

    
