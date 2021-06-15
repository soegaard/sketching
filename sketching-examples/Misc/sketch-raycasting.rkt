#lang sketching
;;; Coding Train #145

(require racket/vector noise)

;;;
;;; Vectors
;;;

(define (norm v)
  (sqrt (+ (sqr v.x) (sqr v.y))))

(define (dist v w)
  (sqrt (+ (sqr (- v.x w.x)) (sqr (- v.y w.y)))))

(define (normalize v)
  (define l (norm v))
  (if (zero? l)
      (vector 0 0)
      (vector-scale (/ 1 l) v)))

(define (vector-scale s v)
  (vector (* s v.x) (* s v.y)))

(define (vector-from-angle angle)
  (vector (cos angle) (sin angle)))

;;;
;;; Particles
;;;

(struct particle (pos rays))

(define (new-particle)
  (define pos  (vector (/ width 2) (/ height 2)))
  (define rays (for/vector ([a (in-range 0 360 1)])
                 (new-ray pos (radians a))))
  (particle pos rays))

(define (particle-update p x y)
  (:= p.pos.x x)
  (:= p.pos.y y))

#;(define (particle-look p walls)
  (define closest #f)
  (define record  +inf.0)
  (for ([ray (in-vector p.rays)])
    (for ([wall (in-vector walls)])
      (define pt (ray-cast ray wall))
      (when pt
        (define d (dist p.pos pt))
        (when (< d record)
          (:= record d)
          (:= closest pt)))))
  (when closest
    (stroke 255 0 0)
    (line p.pos.x p.pos.y closest.x closest.y)))


(define (particle-look p walls)
  (define closest
    (for*/fold ([closest #f] [record +inf.0] #:result closest)
               ([ray  (in-vector p.rays)]
                [wall (in-vector walls)])
      (define pt (ray-cast ray wall))
      (define d  (and pt (dist p.pos pt)))
      (if (and pt (< d record)) 
          (values pt      d)
          (values closest record))))
  
  (when closest
    (stroke 255 0 0)
    (line p.pos.x p.pos.y closest.x closest.y)))

(define (particle-show p)
  (fill 255)
  (ellipse p.pos.x p.pos.y 4)
  (for ([r p.rays])
    (ray-show r)))

;;;
;;; Rays
;;;

(struct ray (pos dir))

(define (new-ray pos angle)
  (ray pos (vector-from-angle angle)))

(define (ray-look-at r x y)
  (:= r.dir.x (- x r.pos.x))
  (:= r.dir.y (- y r.pos.y))
  (:= r.dir   (normalize r.dir)))

(define (ray-show r)
  (stroke 255)
  #;(push)
  (push-matrix)
  (translate r.pos.x r.pos.y)
  (line 0 0 (* r.dir.x 10) (* r.dir.y 10))
  (pop-matrix)
  #;(line r.pos.x r.pos.y
        (+ r.pos.x (* r.dir.x 10))
        (+ r.pos.y (* r.dir.y 10)))

  ; (translate (- r.pos.x) (- r.pos.y))
  #;(pop))

(define (ray-cast r wall)
  (define x1 wall.a.x)
  (define y1 wall.a.y)
  (define x2 wall.b.x)
  (define y2 wall.b.y)

  (define x3 r.pos.x)
  (define y3 r.pos.y)
  (define x4 (+ r.pos.x r.dir.x))
  (define y4 (+ r.pos.y r.dir.y))

  (define den (- (* (- x1 x2) (- y3 y4))
                 (* (- y1 y2) (- x3 x4))))
  (cond
    [(= den 0) #f]
    [else      (define t (/ (- (* (- x1 x3) (- y3 y4))
                               (* (- y1 y3) (- x3 x4)))
                            den))
               (define u (- (/ (- (* (- x1 x2) (- y1 y3))
                                  (* (- y1 y2) (- x1 x3)))
                               den)))
               (if (and (< 0 t 1) (> u 0))
                   (vector (+ x1  (* t (- x2 x1)))
                           (+ y1  (* t (- y2 y1))))
                   #f)]))

;;;
;;; Boundary
;;;

(struct boundary (a b))

(define (new-boundary x1 y1 x2 y2)
  (boundary (vector x1 y1)
            (vector x2 y2)))

(define (boundary-show t)
  (stroke 255)
  (line t.a.x t.a.y t.b.x t.b.y))


;;;
;;; Sketch
;;;

(define walls #f)
(define p     (new-particle))
(define xoff  0)
(define yoff  10000)

(define (setup)
  (size 400 400)
  (frame-rate 2)
  (:= walls (for/vector ([i 5])
              (define x1 (random width))
              (define x2 (random width))
              (define y1 (random height))
              (define y2 (random height))
              (new-boundary x1 y1 x2 y2)))
  (:= walls (vector-append walls
                           (vector (new-boundary 0  0 width 0)
                                   (new-boundary width 0 width height)
                                   (new-boundary width height 0 height)
                                   (new-boundary 0 height 0 0)))))

(define (noise x)
  (perlin x))


(define (draw)
  (background 0)
  (stroke 255)
  (stroke-weight 2)
  (for ([w walls])
    (boundary-show w))
  (particle-update p
                   (+ p.pos.x (* (noise xoff) width))
                   (+ p.pos.y (* (noise yoff) height)))
  (particle-show    p)
  (particle-look p walls)
  (+= xoff 0.001)
  (+= yoff 0.001))
