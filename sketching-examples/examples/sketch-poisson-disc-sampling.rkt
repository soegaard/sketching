#lang sketching
; https://thecodingtrain.com/CodingChallenges/033-poisson-disc.html
; https://www.cct.lsu.edu/~fharhad/ganbatte/siggraph2007/CD2/content/sketches/0250.pdf

(require racket/math data/gvector) ; growable vector
(require (only-in metapict
                  pt vec vec+ pt+ pt-x pt-y dist))

(define (random-vec [r 1])
  ; return random vec(tor) of length r
  (define angle (random (* 2 pi)))
  (vec (* r (cos angle)) (* r (sin angle))))

(define (random-pt [r 1])
  (pt+ (pt 0 0) (random-vec r)))


(define ref vector-ref)
(define set vector-set!)

(define WIDTH  400)
(define HEIGHT 400)

(define r 10) ; radius
(define k 30) ; number of tries before an active point is removed
(define w (/ r (sqrt 2)))

;; STEP 0
(define cols (exact-floor (/ WIDTH w)))
(define rows (exact-floor (/ HEIGHT w)))

(define grid (make-vector (* cols rows) -1)) ; -1 no sample, >=0 index of sample in ordered
(define (grid-ref i j)   (vector-ref  grid (+ j (* i cols))))
(define (grid-set i j x) (vector-set! grid (+ j (* i cols)) x))

(define active  (make-gvector))
(define ordered (make-gvector))
(define (get gv i)         (gvector-ref   gv i))
(define (count gv)         (gvector-count gv))
(define (add-active!  x)   (gvector-add! active  x))
(define (add-ordered! x)   (gvector-add! ordered x))
(define (remove-active! i)
  (define n (count active))
  (unless (= n 1)
    (gvector-set! active i (get active (- n 1))))
  (gvector-remove-last! active))


;; STEP 1
(define x   (/ WIDTH  2))           ; (x,y) ought to be random...
(define y   (/ HEIGHT 2))
(define j   (exact-floor (/ x w)))
(define i   (exact-floor (/ y w)))
(define pos (pt x y))
(grid-set i j 0)
(add-active!  pos)
(add-ordered! pos)


(define (setup)
  (size WIDTH HEIGHT)
  (stroke-weight 4)
  (color-mode 'hsb)
  (background 50 50 255))


(define (draw)
  (for ([total 25]) 
    (unless (zero? (count active))
      (define rand-index (exact-floor (random (count active))))      
      (define pos        (get active rand-index))
      ; search for a point near pos
      (define found #f)
      (for ([n k])
        (define sample (pt+ pos (random-vec (random r (* 2 r)))))
        (define col    (exact-floor (/ (pt-x sample) w)))
        (define row    (exact-floor (/ (pt-y sample) w)))
        (when (and (< -1 col cols) (< -1 row rows)
                   (= (grid-ref row col) -1))
          ; check that all neighbors are more than r away
          (define ok #t)
          (for ([i (in-range -1 2)])
            (for ([j (in-range -1 2)])
              (when (and (< -1 (+ row i) rows)
                         (< -1 (+ col j) cols))
                (define idx (grid-ref (+ row i) (+ col j)))
                (unless (= idx -1)
                  (define neighbor (get ordered idx))
                  (define d (dist sample neighbor))
                  (when (< d r)
                    (set! ok #f))))))
          (when ok
            (set! found #t)
            (define idx (count ordered))
            (grid-set row col idx)
            (add-active!  sample)
            (add-ordered! sample)
            ;; break
            )))
      (unless found
        ; we did not find any new points, so we remove it from active points
        (remove-active! rand-index))))
  (stroke-weight (/ r 2))
  (stroke 0 0 255 0)
  (for ([i (count ordered)])
    ; (stroke (remainder i 360) 100 100)
    (define p (get ordered i))
    ; (circle (pt-x p) (pt-y p) (/ r 2))
    (point (pt-x p) (pt-y p)))
  (stroke 255 0 0)
  (for ([i (count active)])
    ; (stroke (remainder i 360) 100 100)
    (define p (get active i))
    (point (pt-x p) (pt-y p))
    #;(circle (pt-x p) (pt-y p) (/ r 2))))

