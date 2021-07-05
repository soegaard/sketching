#lang sketching
;;;
;;; Diamond Square Algorithm
;;;

; The diamond square algorithm is used to produce height maps.
; Here we map the height to color via the palette function amd
; gets a colorfull map. 

; The original paper "Computer Rendering of Stochastic Models"
; is written by Alain Fournier, Don Fussell and Loren Carpenter.

; The "original" demo Plasma on pc used this algorithm.

; To "animate" we simply change the palette.

; There are some nice illustrations of the algorithm on Wikipedia.


(define N 5)
(define W (+ (expt 2 N) 1))  ; width  in cells
(define H W)                 ; height in cells
(define W- (- W 1))
(define H- (- H 1))
(define s 16)                ; size of each cell on screen

(define array (for/vector ([_ (* W H)]) 0))

(define (idx  i j)            (+ (* i W) j))
(define (ref  i j) (array.ref (+ (* i W) j)))
(define (ref0 i j) (if (and (<= 0 i (- H 1)) (<= 0 j (- W 1)))
                       (ref i j)
                       0.))

(define (initial-corners)
  (:= array (idx 0  0)  (random -1 1))
  (:= array (idx W- 0)  (random -1 1))
  (:= array (idx 0  H-) (random -1 1))
  (:= array (idx W- H-) (random -1 1)))

(define (do-square i j d)
  ; (displayln (list 'square i j d))
  (define i+d (+ i d))
  (define j+d (+ j d))
  (define avg (* 0.25 (+ (ref i j)   (ref i+d j)
                         (ref i j+d) (ref i+d j+d))))
  (define r (random 2))
  (:= array (idx (+ i (/ d 2)) (+ j (/ d 2)))
      ; note: the amount of added noise is reduced from round to rount
      (+ avg (/ (random -0.5 0.5)
                (expt 1.4 d)))))

(define (do-diamond i j d) ; (i,j) center of diamond
  (define i+d (modulo (+ i d) W))
  (define j+d (modulo (+ j d) H))
  (define i-d (modulo (- i d) W))
  (define j-d (modulo (- j d) H))
  ; since ref is used, out of bounds indices are wrapped
  (define avg (* 0.25 (+ (ref i-d j)   (ref i+d j)
                         (ref i   j-d) (ref i   j+d))))  
  (:= array (idx i j)
      ; note: the amount of added noise is reduced from round to round
      (+ avg (/ (random -0.5 0.5)
                (expt 1.4 d)))))

(define (do-diamond0 i j d) ; (i,j) center of diamond
  (define i+d (+ i d))
  (define j+d (+ j d))
  (define i-d (- i d))
  (define j-d (- j d))
  ; Since ref0 is used, out of bounds values are 0
  (define avg (* 0.25 (+ (ref0 i-d j)   (ref0 i+d j)
                         (ref0 i   j-d) (ref0 i   j+d))))  
  (:= array (idx i j)
      (+ avg (random -0.5 0.5))))

(define (do-diamonds i j d) ; (i,j) upper, left corner
  (define d/2 (/ d 2))
  (do-diamond    i      (+ j d/2)  d/2)
  (do-diamond (+ i d/2)    j       d/2)
  (do-diamond (+ i d/2) (+ j d)    d/2)
  (do-diamond (+ i d)   (+ j d/2)  d/2))

(define (do-diamonds0 i j d) ; (i,j) upper, left corner
  (define d/2 (/ d 2))
  (do-diamond0    i      (+ j d/2)  d/2)
  (do-diamond0 (+ i d/2)    j       d/2)
  (do-diamond0 (+ i d/2) (+ j d)    d/2)
  (do-diamond0 (+ i d)   (+ j d/2)  d/2))


(define (palette x)
  ; convert the height to a hue angle from 0 to 360 degree.
  (define hue0 (int (* 360 (remap x -1. 1. 0. 1.))))
  ; use the frame-count to rotate the hue angle,
  ; the factor determines the speed of color change
  (define hue (remainder (+ hue0 (* 4 frame-count)) 360))
  (define sat 50)
  (define bri 100)
  (color hue sat bri))

(define (init i j d)  
  (define d/2 (/ d 2))
  (unless (<= d 1)
    (do-square i j d)
    (do-diamonds i j d)     ; wraps around
    ; (do-diamonds0 i j d)  ; uses zeros for out of boundsa    
    (for* ([r (in-range i (+ i d) d/2)]
           [c (in-range j (+ j d) d/2)])
      (init r c d/2))))
             
(define (setup)  
  (size (max 100 (* s W)) (max 100 (* s H)))
  (frame-rate 30)
  (color-mode 'hsb 360 100 100 100)
  ; calculate the height map once only
  (initial-corners)
  (init 0 0 (expt 2 N)))
  

(define (draw)
  ; A black stroke around each cell looks nice
  ;   (stroke-weight 1) (stroke 0)
  ; but it's slower than no borders.
  (no-stroke)
  (for* ([i H] [j W])
    (fill (palette (ref i j)))
    (rect (* s j) (* s i) s s)))


