#lang racket
(require sketching)

(struct particle (x y vx vy ax ay α) #:transparent)

(define (not-black? p)
  (> (particle-α p) 0))

(define (update-particle p)
  (match-define (particle x y vx vy ax ay α) p)
  (particle (+ x vx) (+ y vy) (+ vx ax) (+ vy ay) ax ay (max (- α 2) 0)))

(define (new-particle [x 300] [y 300] [vx (random -1 1)] [vy (random -5 -1)] [ax 0] [ay 0.1])
  (particle x y vx vy ax ay 255))

(define (draw-particle p)
  (match-define (particle x y vx vy ax ay α) p)
  (fill 255 α)
  (circle x y 5))

(define particles '())

(define (add-particle! p)
  (set! particles (cons p particles)))

(define (update-particles!)
  (set! particles
        (filter not-black?
                (map update-particle particles))))

(define (setup)
  (frame-rate 120)
  (size 600 400))

(define (mouse-pressed)
  (for ([i 10])
    (add-particle! (new-particle mouse-x mouse-y))))


(define (draw)  
  (background 0)
  (no-stroke)
  (map draw-particle particles)
  (update-particles!)
  (add-particle! (new-particle 300 200)))


(setup)
(current-draw draw)
(current-on-mouse-pressed mouse-pressed)
(start)
