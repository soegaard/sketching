#lang racket/base
(provide alpha
         arc
         background
         blue
         circle
         color
         color-lerp
         ellipse
         ellipse-mode
         fill
         green
         image
         line
         load-image
         no-stroke
         no-fill
         point
         quad
         rect
         rect-mode
         red
         stroke
         stroke-weight
         stroke-join
         stroke-cap
         triangle
         )

(require racket/draw
         racket/class
         racket/match
         racket/format
         "color.rkt"
         "parameters.rkt"
         "math.rkt")


;;;
;;; Coordinates
;;;

; Cartesian coordinate system. The origin (0,0) is in the upper-left corner.
; Pixels are drawn to the right and below a coordiate.

;;;
;;; 2d Primitives
;;;

(define (arc x y w h start stop [mode #f])
  ; x,y          center
  ; w,h          width, height
  ; start, stop  in radians
  ; mode         todo ignored for now
  (send dc draw-arc x y w h start stop))

(define (circle x y extent)
  (ellipse x y extent extent))

(define (ellipse x y w h)
  ; x,y          center
  ; w,h          width, height

  (define mode (current-ellipse-mode))
  (case mode
    [(corner)  (send dc draw-ellipse x y w h)] ; x,y is upper left corner
    [(corners) (define-values (x0 y0 x1 y1) (values (min x w) (min y h) (max x w) (max y h)))
               (send dc draw-ellipse x0 y0 (- x1 x0) (- y1 y0))] ; two opposite corners
    [(center)  (define-values (x0 y0 w0 h0) (values (- x (/ w 2.)) (- y (/ h 2.)) w h))
               (send dc draw-ellipse x0 y0 w0 h0)]
    [(radius)  (define-values (x0 y0) (values (- x w) (- y h)))
               (send dc draw-ellipse x0 y0 (* 2. w) (* 2. h))]
    [else      (error 'ellipse "internal error: unsupported ellipse mode, got: ~a" mode)]))

(define (line x0 y0 x1 y1)
  (send dc draw-line x0 y0 x1 y1))

(define (point x y)
  (send dc draw-point x y))

(define (quad x1 y1 x2 y2 x3 y3 x4 y4)
  (send dc draw-polygon (list (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4))))


(define (rect x y w h [r #f])
  ; TODO: implement support for rounded cornes with different radii
  ; x,y          center
  ; w,h          width, height
  (define mode (current-rect-mode))
  (define (draw x y w h)
    (if r
        (send dc draw-rounded-rectangle x y w h r)
        (send dc draw-rectangle         x y w h)))  
  (case mode
    [(corner)  (draw x y w h)] ; x,y is upper left corner
    [(corners) (define-values (x0 y0 x1 y1) (values (min x w) (min y h) (max x w) (max y h)))
               (draw x0 y0 (- x1 x0) (- y1 y0))] ; two opposite corners
    [(center)  (define-values (x0 y0 w0 h0) (values (- x (/ w 2.)) (- y (/ h 2.)) w h))
               (draw x0 y0 w0 h0)]
    [(radius)  (define-values (x0 y0) (values (- x w) (- y h)))
               (draw x0 y0 (* 2. w) (* 2. h))]
    [else      (error 'rect "internal error: unsupported ellipse mode, got: ~a" mode)]))

(define (square x y extent)
  (rect x y extent extent))

(define (triangle x1 y1 x2 y2 x3 y3)
  (send dc draw-polygon (list (cons x1 y1) (cons x2 y2) (cons x3 y3))))

;;;
;;; Attributes
;;;

(define (ellipse-mode mode)
  (unless (member mode '(center radius corner corners))
    (error 'ellipse-mode "expected one of 'center 'radius 'corner or 'corners, got: ~a" mode))
  (current-ellipse-mode mode))

(define (rect-mode mode)
  (unless (member mode '(center radius corner corners))
    (error 'rect-mode "expected one of 'center 'radius 'corner or 'corners, got: ~a" mode))
  (current-rect-mode mode))

(define (stroke-weight weight)
  (define pen     (send dc get-pen))
  (define new-pen (new pen%
                       [color   (send pen get-color)]
                       [width   weight]
                       [style   (send pen get-style)]
                       [cap     (send pen get-cap)]
                       [join    (send pen get-join)]
                       [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))


(define (stroke-join join)
  ; join is one of 'miter, 'bevel, 'round
  (define pen     (send dc get-pen))
  (define new-pen (new pen%
                       [color   (send pen get-color)]
                       [width   (send pen get-width)]
                       [style   (send pen get-style)]
                       [cap     (send pen get-cap)]
                       [join    join]
                       [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))


(define (stroke-cap cap)
  ; cap is one of 'round 'projecting 'butt
  (define pen     (send dc get-pen))
  (define new-pen (new pen%
                       [color   (send pen get-color)]
                       [width   (send pen get-width)]
                       [style   (send pen get-style)]
                       [cap     cap]
                       [join    (send pen get-join)]
                       [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))



;;;
;;; Color
;;;

; See also "color.rkt"

(define (exact-round x)
  (inexact->exact (round x)))


(define (stroke . args)
  (define col     (args->color args 'stroke))
  (define pen     (send dc get-pen))
  (define new-pen (new pen%
                       [color   col]
                       [width   (send pen get-width)]
                       [style   (send pen get-style)]
                       [cap     (send pen get-cap)]
                       [join    (send pen get-join)]
                       [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))

(define (no-stroke)
  (define pen (send dc get-pen))
  (define c   (send pen get-color))
  (stroke (red c) (green c) (blue c) 0.))

(define (fill . args)
  (define col       (args->color args 'fill))
  (define brush     (send dc get-brush))
  (define new-brush (new brush%
                         [color          col]
                         [style          (send brush get-style)]
                         [stipple        (send brush get-stipple)]
                         [gradient       (send brush get-gradient)]
                         [transformation (send brush get-transformation)]))  
  (send dc set-brush new-brush))

(define (no-fill)
  (define b (send dc  get-brush))
  (define c (send b   get-color))
  (fill (red c) (green c) (blue c) 0.))

(define (background . args)
  ; https://processing.org/reference/background_.html
  ; todo: allow image as background
  (define col (args->color args 'background))
  (send dc set-background col)
  (send dc clear))

;;;
;;; Images
;;;

(define (load-image path)
  (make-object bitmap%	path))

(define (image bitmap x y)
  (define mode (current-rect-mode))
  (define (draw x y w h)
    (send dc draw-bitmap bitmap x y))
  (define w (send bitmap get-width))
  (define h (send bitmap get-height))
  (case mode
    [(corner)  (draw x y w h)] ; x,y is upper left corner
    [(corners) (define-values (x0 y0 x1 y1) (values (min x w) (min y h) (max x w) (max y h)))
               (draw x0 y0 (- x1 x0) (- y1 y0))] ; two opposite corners
    [(center)  (define-values (x0 y0 w0 h0) (values (- x (/ w 2.)) (- y (/ h 2.)) w h))
               (draw x0 y0 w0 h0)]
    [(radius)  (define-values (x0 y0) (values (- x w) (- y h)))
               (draw x0 y0 (* 2. w) (* 2. h))]
    [else      (error 'image "internal error: unsupported image mode, got: ~a" mode)]))

(define (image-mode mode)
  (unless (member mode '(center radius corner corners))
    (error 'image-mode "expected one of 'center 'radius 'corner or 'corners, got: ~a" mode))
  (current-image-mode mode))

  



    
    
         
