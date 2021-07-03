#lang racket/base
(provide alpha
         arc
         background
         begin-shape
         bezier
         blue
         circle
         color
         create-image
         ellipse
         ellipse-mode
         end-shape
         fill
         green
         image
         image-mode
         image-get
         image-set
         image-width
         image-height         
         lerp-color
         line
         load-image
         no-fill
         no-stroke
         no-tint
         no-smooth
         point
         quad
         rect
         rect-mode
         red
         smoothing  ; Racket only
         square
         stroke
         stroke-weight
         stroke-join
         stroke-cap
         text
         text-align
         text-size
         text-face
         text-family
         text-weight
         text-underlined?
         text-smoothing
         text-size-in-pixels?
         text-hinting
         tint
         triangle
         vertex
         )

(require racket/draw
         racket/class
         racket/match
         racket/format
         racket/list
         "color.rkt"
         "parameters.rkt"
         "math.rkt"
         "typography.rkt")

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
  ; start, stop  in radians (clockwise [sigh])
  ; mode         todo ignored for now

  ; x,y upper left, start=0 is three o'clock and half-pi is twelve o'clock. 
  ; (send dc draw-arc x y w h start stop) 
  (define ellipse-mode (current-ellipse-mode))
  (define from  stop)
  (define to    start)
  (define delta (- stop start))
  (set! to      (- (* 2 pi) to))
  (set! from    (- to delta))
  (define h/2 (/ h 2.))
  (define w/2 (/ w 2.))    

  (define (draw-arc x y w h from to)
    ; (displayln (list 'draw-arc x y w h from to))
    (cond
      [(current-fill)
       (define path (new dc-path%))
       (case mode
         [(#f open-pie)
          (send path move-to (+ x w/2) (+ y h/2)) ; center
          (send path arc x y w h from to)
          (send path close)
          ; fill segment without stroke
          (define old-pen (send dc get-pen))
          (send dc set-pen the-transparent-pen)
          (send dc draw-path path)
          (send dc set-pen old-pen)
          ; draw stroke
          (send dc draw-arc x y w h from to)]
         [(open)
          (send path arc x y w h from to)
          (send path close)
          ; fill segment without stroke
          (define old-pen (send dc get-pen))
          (send dc set-pen the-transparent-pen)
          (send dc draw-path path)
          (send dc set-pen old-pen)
          ; draw stroke
          (send dc draw-arc x y w h from to)]
         [(chord)
          (send path arc x y w h from to)
          (send path close)
          (send dc draw-path path)]
         [(pie)
          (send path move-to (+ x w/2) (+ y h/2)) ; center
          (send path arc x y w h from to)
          (send path close)
          (send dc draw-path path)])]
      [else
       ;; no fill here
       (define path (new dc-path%))
       (send path arc x y w h from to)
       (send dc draw-path path)]))
  
  (case ellipse-mode
    [(corner)  (draw-arc x y w h from to)] ; x,y is upper left corner
    [(corners) (define-values (x0 y0 x1 y1) (values (min x w) (min y h) (max x w) (max y h)))
               (draw-arc x0 y0 (- x1 x0) (- y1 y0) from to)] ; two opposite corners
    [(center)  (define-values (x0 y0 w0 h0) (values (- x (/ w 2.)) (- y (/ h 2.)) w h))
               (draw-arc x0 y0 w0 h0 from to)]
    [(radius)  (define-values (x0 y0) (values (- x w) (- y h)))
               (draw-arc x0 y0 (* 2. w) (* 2. h) from to)]
    [else      (error 'arc "internal error: unsupported ellipse mode, got: ~a" mode)]))


(define (circle x y extent)
  (define mode (current-ellipse-mode))
  (case mode
    ; Both P. and p5js will draw an ellipse when the ellipse mode is corners,
    ; so let's keep the same behaviour.
    #;[(corners) (define-values (x0 y0 x1 y1) (values (min x w) (min y h) (max x w) (max y h)))
                 (draw-ellipse x0 y0 (+ x0 extent) (+ y0 extent))]
    [else
     (ellipse x y extent extent)]))


(define (ellipse x y w [h w])
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
  ; Note: For unsmoothed this gives better results (symetrical points!) than
  ;       using (send dc draw-point x y).
  ;(define r (send (send dc get-pen) get-width))
  ; (line x y (+ x 0.0000000001) (+ y 0.0000000001))
  (line x y (+ x 0.00000001) (+ y 0.00000001))
  ; (send dc draw-point x y)
  )

(define (quad x1 y1 x2 y2 x3 y3 x4 y4)
  (send dc draw-polygon (list (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4))))

(define (bezier x1 y1 x2 y2 x3 y3 x4 y4)
  (define p (new dc-path%))
  (send p move-to x1 y1)
  (send p curve-to x2 y2 x3 y3 x4 y4)
  (send dc draw-path p))


(define (rect-mode-args->top-left+width+height mode x1 y1 x2 y2)
  (case mode
    [(corner)  (values x1 y1 x2 y2)]
    [(corners) (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))]
    [(center)  (define-values (w h) (values x2 y2))
               (values (- x1 (/ w 2.)) (- y1 (/ h 2.)) w h)]
    [(radius)  (define-values (w h) (values (* 2 x2) (* 2 y2)))
               (values (- x1 x2) (- y1 y2) w h)]
    [else      (error rect-mode-args->top-left+width+height
                      (~a "internal error, got: ~a" mode))]))



(define (rect x y w h [r #f])
  ; TODO: implement support for rounded cornes with different radii
  ; x,y          center
  ; w,h          width, height
  (define mode (current-rect-mode))
  (define (sketching-draw-rectangle x y w h)
    ; Note: (send dc draw-rectangle x y w h)
    ;       Doesn't works as expected - due to alignment it shrinks
    ;       the rectangle - and two rectangles side by side won't share the edge.
    (define pen     (send dc get-pen))
    (define new-pen (if (eq? (send pen get-cap) 'butt)
                        pen
                        (new pen%
                             [color   (send pen get-color)]
                             [style   (send pen get-style)]
                             [width   (send pen get-width)]
                             [cap     'butt]
                             [join    (send pen get-join)]
                             [stipple (send pen get-stipple)])))
    ; We use a path, so filling works.
    (define path (new dc-path%))
    (send path move-to    x       y)
    (send path line-to (+ x w)    y)
    (send path line-to (+ x w) (+ y h))
    (send path line-to    x    (+ y h))
    (send path line-to    x       y)
    (send path close)
    
    (send dc set-pen new-pen)
    (send dc draw-path path)
    (send dc set-pen pen))
  
  (define (draw x y w h)
    (if r
        (send dc draw-rounded-rectangle x y w h r)
        (sketching-draw-rectangle x y w h)))
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
  (unless (memq join '(miter bevel round))
    (raise-argument-error 'stroke-join "one of the symbols: miter bevel round" join))
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
  (when (eq? cap 'project) (set! cap 'projecting))
  (when (eq? cap 'square)  (set! cap 'butt))
  
  (unless (memq cap '(round projecting butt))
    (raise-argument-error 'stroke-cap "one of the symbols: round projecting butt" cap))
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
  (define col       (args->color args 'stroke))
  (define pen       (send dc get-pen))
  (define style     (send pen get-style))
  (define new-style (if (eq? style 'transparent) 'solid style))
  (define new-pen   (new pen%
                         [color   col]
                         [width   (send pen get-width)]
                         [style   new-style]
                         [cap     (send pen get-cap)]
                         [join    (send pen get-join)]
                         [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))

(define (no-stroke)
  (define pen     (send dc get-pen))
  (define new-pen (new pen%
                       [color   (send pen get-color)]
                       [width   (send pen get-width)]
                       [style   'transparent]
                       [cap     (send pen get-cap)]
                       [join    (send pen get-join)]
                       [stipple (send pen get-stipple)]))
  (send dc set-pen new-pen))

(define (fill . args)
  (define col       (args->color args 'fill))
  (define brush     (send dc get-brush))
  (define style     (send brush get-style))
  (define new-style (if (eq? style 'transparent) 'solid style))
  (define new-brush (new brush%
                         [color          col]
                         [style          new-style]
                         [stipple        (send brush get-stipple)]
                         [gradient       (send brush get-gradient)]
                         [transformation (send brush get-transformation)]))
  (current-fill col)
  (send dc set-brush new-brush))


(define (no-fill)
  (define brush     (send dc get-brush))
  (define new-brush (new brush%
                         [color          (send brush get-color)]
                         [style          'transparent]
                         [stipple        (send brush get-stipple)]
                         [gradient       (send brush get-gradient)]
                         [transformation (send brush get-transformation)]))
  (current-fill #f)
  (send dc set-brush new-brush))

(define (tint . args)
  (define col       (args->color args 'tint))
  (define brush     (send dc get-brush))
  (define style     (send brush get-style))
  (define new-style (if (eq? style 'transparent) 'solid style))
  (define new-brush (new brush%
                         [color          col]
                         [style          new-style]
                         [stipple        #f]
                         [gradient       #f]
                         [transformation #f]))
  (current-tint-color col)
  (current-tint-brush new-brush))

(define (no-tint)
  (current-tint-color #f))


(define (background . args)
  ; https://processing.org/reference/background_.html
  ; todo: allow image as background
  (define col (args->color args 'background))
  (define reg (send dc get-clipping-region))
  (send dc set-clipping-region #f)
  ;(define old (send dc get-transformation))
  ;(send dc set-transformation (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
  (send dc set-background col)
  (send dc clear)
  ; (send dc set-transformation old)
  (send dc set-clipping-region reg))

(define (smoothing mode)
  (send dc set-smoothing mode))

(define (no-smooth)
  (smoothing 'unsmoothed))


;;;
;;; Images
;;;

(define (load-image path)
  (make-object bitmap% path))

(require cairo)

(define the-pen-list        (new pen-list%))
(define the-transparent-pen (send the-pen-list find-or-create-pen (make-object color% 0 0 0) 0 'transparent))
(define the-black-color     (send the-color-database find-color "black"))
(define the-black-pen       (send the-pen-list find-or-create-pen (make-object color% 0 0 0) 0 'solid))

(define (image bitmap x y)
  (define mode (current-image-mode))
  (define (draw x y w h)
    (cond
      [(current-tint-color)
       ; 1. Create temporary bitmap
       (define tmp-bitmap (make-bitmap w h)) ; color-alpha, argb32
       (define tmp-dc     (new bitmap-dc% [bitmap tmp-bitmap]))
       ; 2. Make a copy of our original bitmap
       (send tmp-dc draw-bitmap bitmap 0 0
             'solid the-black-color ; ignored
             bitmap) ; mask (i.e. respect transparency)

       ; 3. Tint the temporary copy
       (define target  (new ImageSurface [bitmap tmp-bitmap]))
       (define context (new Context      [target target]))
       ; TODO TODO:  use actual tint color here
       (send context set-source-rgba 0. 0. 0. 0.5) ; red green blue alpha) ; in the range 0.0 to 1.0
       (send context set-operator 'atop)
       (send context paint)

       ; 4. Draw the copy
       (send dc draw-bitmap tmp-bitmap x y 'solid the-black-color tmp-bitmap)
       
       ;; (send tmp-dc draw-bitmap bitmap 0 0)       
       ;; (define tint-brush (current-tint-brush))
       ;; (when tint-brush
       ;;   (send tmp-dc set-brush tint-brush)
       ;;   (send tmp-dc set-pen   the-transparent-pen)
       ;;   (send tmp-dc draw-rectangle 0 0 w h))       ;; (send dc draw-bitmap tmp-bitmap x y)]
       ]
      [else
       (send dc draw-bitmap bitmap x y 'solid the-black-color bitmap)]))
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


(define (create-image w h format)
  ; format is one of rgb, argb, alpha
  (make-bitmap (* 2 w) (* 2 h)))

(define (image-set bm x y c)
  ; todo: implement support for c being an image
  (set! c (args->color (list c) 'image-set))
  (define pixels (bytes 255 (red255 c) (green255 c) (blue255 c)))
  (when (and (>= x 0) (>= y 0))
    (send bm set-argb-pixels x y 1 1 pixels)))

(define (image-get bm x y)
  (define pixels (bytes 0 0 0 0))  
  (send bm get-argb-pixels x y 1 1 pixels)
  (color (bytes-ref pixels 1)
         (bytes-ref pixels 2)
         (bytes-ref pixels 3)
         (bytes-ref pixels 0)))

(define (image-width bm)
  (send bm get-width))

(define (image-height bm)
  (send bm get-height))
    



;;;
;;; Text
;;;

(define (text-align hor [ver 'baseline])
  (current-text-horizontal-align hor)
  (current-text-vertical-align   ver))


(define (text . args)
  ; There are basically three scenarios:
  ;   1. Contents and (x,y) are given.
  ;   2. Contents and (x,y) and width are given.
  ;   3. Contents and (x,y) and width and height are given.

  ; Scenario 3
  ;   With string and four arguments, we can use rect-mode to interpret the arguments
  ;   in order to get the width and height of the rectangle.
  ;   The linewidth is present, so within in the text rectangle
  ;   text can be aligned left, center and right.

  ;   Note: The vertical alignment is ignored in scenario 3 (we take it as 'top).

  ; Scenario 2 - TODO - Not in P.
  ;   The line width is present, so text can be aligned left, center and right.
  ;   Note: The vertical alignment is ignored in scenario 3 (we take it as 'top).

  ; Scenario 1
  ;   Both horizontal and vertical alignment is taken into consideration.
  
  (define mode (current-rect-mode)) ; sigh... 
  (match args
    ;; Scenario 1:  No width and height present.
    ;              Single line.
    [(list (? string? s) (? number? x) (? number? y))
     (case mode
       [(corner corners radius)  (do-text-line s x y)] 
       [(center)                 (parameterize ([current-text-horizontal-align 'center]
                                                [current-text-vertical-align   'center])
                                   (do-text-line s x y))]
       [else      (error 'text "internal error: unsupported rect mode, got: ~a" mode)])]
    
    ; characters
    [(list (? char? c) (? number? x) (? number? y))
     (text (string c) x y)]
    
    [(list (list (? string? cs) ...) (? number? x) (? number? y))
     (text (list->string cs) x y)]

    ;; Scenario  3
    [(list contents (? number? x1) (? number? y1) (? number? x2) (? number? y2))
     ; Calculate top-left (x,y) and the width and height according to rect-mode.
     (define-values (x0 y0 w h) (rect-mode-args->top-left+width+height mode x1 y1 x2 y2))
     (do-text contents x0 y0 w h)]
    [_
     (error 'text "got: ~a" (~a (map ~a args)))]))

(define (do-text-line s x y)
  ;; Scenario 1 - no given width or height
  ;;   single line
  (define old-color (send dc get-text-foreground))
  (define old-mode  (send dc get-text-mode))       ; transparent or solid
  (define brush     (send dc get-brush))
  (define color     (send brush get-color))
  (send dc set-text-mode 'transparent)
  (send dc set-text-foreground color)
  ; w=width, h=height, b=dist from baseline to descender, e=extra vertical space

  ; Note: Use "x" with get-text-extent to get a correct value for b.
  ;       If s contains newlines, an incorrect value is returned.
  (define-values ( w  h _b _e) (send dc get-text-extent s))
  (define-values (_w _h  b  e) (send dc get-text-extent "x"))
  
  (define a (- h b))
  (define x0
    (case (current-text-horizontal-align)
      [(left)      x]
      [(right)  (- x    w)]
      [(center) (- x (/ w 2.))]))
  (define y0
    (case (current-text-vertical-align)
      [(top)         y]
      [(bottom)   (- y    h)]
      [(center)   (- y (/ h 2.))]
      [(baseline) (- y a)]))
  (send dc draw-text s x0 y0)  
  (send dc set-text-mode old-mode))

(define (do-text contents x y w h)
  ;; Scenario 3 - width and height are given
  ;;   multiple lines
  (define old-color (send dc get-text-foreground))
  (define old-mode  (send dc get-text-mode))       ; transparent or solid
  (define brush     (send dc get-brush))
  (define color     (send brush get-color))
  (send dc set-text-mode 'transparent)
  (send dc set-text-foreground color)

  (define hor-alignment (current-text-horizontal-align))
  (define leading       2)
  (text/aligned contents dc x y w h hor-alignment leading)
  
  (send dc set-text-mode old-mode))

(define (create-font)
  (make-font #:size            (current-font-size)
             #:face            (current-font-face)
             #:family          (current-font-family)
             #:style           (current-font-style)
             #:weight          (current-font-weight)
             #:underlined?     (current-font-underlined?)
             #:smoothing       (current-font-smoothing)
             #:size-in-pixels? (current-font-size-in-pixels?)
             #:hinting         (current-font-hinting)))

(define (text-size size) ; units of pizels
  (unless (equal? size (current-font-size))
    (current-font-size size)
    (send dc set-font (create-font))))

(define (text-face face)
  (unless (equal? face (current-font-face))
    (current-font-face face)
    (send dc set-font (create-font))))

(define (text-family family)
  (unless (equal? family (current-font-family))
    (current-font-family family)
    (send dc set-font (create-font))))

(define (text-style style)
  (unless (equal? style (current-font-style))
    (current-font-style style)
    (send dc set-font (create-font))))

(define (text-weight weight)
  (unless (equal? weight (current-font-weight))
    (current-font-weight weight)
    (send dc set-font (create-font))))

(define (text-underlined? underlined?)
  (unless (equal? underlined? (current-font-underlined?))
    (current-font-underlined? underlined?)
    (send dc set-font (create-font))))

(define (text-smoothing smoothing)
  (unless (equal? smoothing (current-font-smoothing))
    (current-font-smoothing smoothing)
    (send dc set-font (create-font))))

(define (text-size-in-pixels? size-in-pixels?)
  (unless (equal? size-in-pixels? (current-font-size-in-pixels?))
    (current-font-size-in-pixels? size-in-pixels?)
    (send dc set-font (create-font))))

(define (text-hinting hinting)
  (unless (equal? hinting (current-font-hinting))
    (current-font-hinting hinting)
    (send dc set-font (create-font))))


; A shape is represented as a dc-path% and the number of points
; added to the shape.
(struct shape (kind rev-points draw) #:mutable)
; kind in default, points, lines, triangles, triangle-fan, triangle-strip, quads, quad_strip
; rev-points are points added one at a time by vertex in reverse order.
; When end-shape is called, it will create a draw function, which can be used
; by draw-shape.
; Until end-shape is called, draw will be #f.

(define (new-shape [kind 'default])
  (shape kind '() #f))

(define (make-shape kind rev-points draw)
  (shape kind rev-points draw))

(define (finish-shape shape who [close? #f])
  (define kind       (shape-kind shape))
  (define rev-points (shape-rev-points shape))
  (define points     (reverse rev-points))
  (define x car)
  (define y cdr)
  (if (empty? points)
      (make-shape kind rev-points void)
      (case kind
        [(default)
         (define path (new dc-path%))
         (define p1   (first points))
         (send path move-to (x p1) (y p1))
         (for ([p (rest points)])
           (send path line-to (x p) (y p)))
         (when close?
           (send path close))
         (define (draw dc) (send dc draw-path path))
         (make-shape kind #f draw)]
        [(points)
         (define (draw dc)
           (for ([p points])
             (send dc draw-point (x p) (y p))))
         (make-shape kind #f draw)]
        [(lines)
         (define n (length points))
         ; skip the last point, if there is an odd number of points
         (set! points (if (even? n) points (reverse (rest rev-points))))
         (define (draw dc)
           (let loop ([ps points])
             (unless (empty? ps)
               (define p (first ps))
               (define q (second ps))
               (send dc draw-line (x p) (y p) (x q) (y q))
               (loop (rest (rest ps))))))
         (make-shape kind #f draw)]
        [(triangles)
         (define n (length points))
         (define r (remainder n 3))
         ; skip the r points, if 3 doesn't divide n.         
         (set! points (if (zero? r) points (reverse (drop rev-points r))))
         (define paths
           (let loop ([ps points])
             (if (empty? ps)
                 '()
                 (let ()
                   (define p (first ps))
                   (define q (second ps))
                   (define s (third ps))
                   (define path (new dc-path%))
                   (send path move-to (x p) (y p))               
                   (send path line-to (x q) (y q))
                   (send path line-to (x s) (y s))
                   (send path close)
                   (cons path (loop (rest (rest (rest ps)))))))))           
         (define (draw dc)
           (for ([path paths])
             (send dc draw-path path)))
         (make-shape kind #f draw)]
        [(triangle-strip)
         (define n (length points))
         (define paths 
           (unless (< n 3)
             (let loop ([p  (first points)]
                        [q  (second points)]
                        [ps (rest (rest points))])
               (cond
                 [(empty? ps) '()]
                 [else
                  (define s (first ps))
                  (define path (new dc-path%))
                  (send path move-to (x p) (y p))               
                  (send path line-to (x s) (y s))
                  (send path line-to (x q) (y q))
                  (send path close)
                  (cons path
                        (loop q s (rest ps)))]))))
         (define (draw dc)
           (unless (< n 3)
             (for ([path paths])
               (send dc draw-path path))))
         (make-shape kind #f draw)]
        [(triangle-fan)
         (define n (length points))
         (define paths 
           (unless (< n 3)
             (let loop ([p  (first points)]
                        [q  (second points)]
                        [ps (rest (rest points))])
               (cond
                 [(empty? ps) '()]
                 [else
                  (define s (first ps))
                  (define path (new dc-path%))
                  (send path move-to (x p) (y p))               
                  (send path line-to (x s) (y s))
                  (send path line-to (x q) (y q))
                  (send path close)
                  (cons path
                        (loop p s (rest ps)))]))))
         (define (draw dc)
           (unless (< n 3)
             (for ([path paths])
               (send dc draw-path path))))
         (make-shape kind #f draw)]
        [(quads)
         (define n (length points))
         (define d (remainder n 4))
         ; drop the d points, if 4 doesn't divide n.         
         (set! points (if (zero? d) points (reverse (drop rev-points d))))
         (define paths
           (let loop ([ps points])
             (if (empty? ps)
                 '()
                 (let ()
                   (define p (first ps))
                   (define q (second ps))
                   (define r (third ps))
                   (define s (fourth ps))
                   (define path (new dc-path%))
                   (send path move-to (x p) (y p))               
                   (send path line-to (x q) (y q))
                   (send path line-to (x r) (y r))
                   (send path line-to (x s) (y s))
                   (send path close)
                   (cons path (loop (rest (rest (rest (rest ps))))))))))
         (define (draw dc)
           (for ([path paths])
             (send dc draw-path path)))
         (make-shape kind #f draw)]
        [(quad-strip)
         (define n (length points))
         (define paths 
           (unless (< n 4)
             (let loop ([p  (first points)]
                        [q  (second points)]
                        [ps (rest (rest points))])
               (cond
                 [(empty? ps) '()]
                 [else
                  (define r (first ps))
                  (define s (second ps))
                  (define path (new dc-path%))
                  (send path move-to (x p) (y p))
                  (send path line-to (x s) (y s))                  
                  (send path line-to (x r) (y r))
                  (send path line-to (x q) (y q))
                  (send path close)
                  (cons path
                        (loop r s (rest (rest ps))))]))))
         (define (draw dc)
           (unless (< n 4)
             (for ([path paths])
               (send dc draw-path path))))
         (make-shape kind #f draw)]
        [else
         (error who (~a "internal error: got the kind " kind))])))

(define (draw-shape shape)
  (define draw (shape-draw shape))
  (when draw (draw dc)))

(define (begin-shape [kind 'default])
  (define old-shapes (current-shapes))
  (current-shapes (cons (new-shape kind) old-shapes)))

(define (end-shape [mode #f])
  (define shape (car (current-shapes)))
  (current-shapes (cdr (current-shapes)))  
  (case mode
    [(close) (draw-shape (finish-shape shape 'end-shape #t))]
    [else    (draw-shape (finish-shape shape 'end-shape #f))]))

(define (vertex x y)
  (define shape      (car (current-shapes)))
  (define rev-points (shape-rev-points shape))
  (set-shape-rev-points! shape (cons (cons x y) rev-points)))


   
