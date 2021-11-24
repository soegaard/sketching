#lang racket/base
(require (only-in racket/math sqr pi))

(provide
 ; abs
 ceil
 constrain
 degrees
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
 sqr
 ; constants
 pi   π
 pi/2 π/2
 pi/4 π/4
 2pi 2π
 
 atan2
 round-up)

(define (atan2 y x)
  (atan y x))


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
  (lerp start2 stop2 (/ (- value start1) (- stop1 start1))))

(define (norm value start stop)
  (/ (- value start) (- stop start)))

(define (pow n e)
  (expt n e))


(define (sq  x) (* x x))

; (define pi         3.141592653589793)
(define π pi)

(define pi/2 (/ pi 2))
(define π/2  pi/2)

(define pi/4 (/ pi 4))
(define π/4  pi/4)

(define 2pi (* 2 pi))
(define 2π  2pi)


(define r/d (/ 3.141592653589793 180.))
(define (radians d)
  (* r/d d))

(define d/r (/ 180. 3.141592653589793))
(define (degrees r)
  (* d/r r))

(define new-random
  (case-lambda
    [()         (random)]
    [(high)     (* high (random))]
    [(low high) (+ low (* (- high low) (random)))]))
    
(define (round-up x)
  ; round ties towards +inf.0
  (floor (+ x 0.5)))
