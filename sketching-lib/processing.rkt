#lang racket/base
(require "color.rkt"
         "conversion.rkt"
         "environment.rkt"
         "gui.rkt"
         "graphics.rkt"
         "math.rkt"
         "parameters.rkt"
         "time-and-date.rkt"
         "transform.rkt")

(provide
 ; Temporarily (until we make a language)
 current-draw
 current-on-mouse-pressed
 current-on-mouse-released
 current-on-mouse-moved
 current-on-mouse-dragged

 current-key
 current-key-pressed
 current-key-released
 current-on-key-pressed
 current-on-key-released

 ;; Permanent 
 alpha
 arc
 background
 blue
 circle
 color
 color?
 color-lerp
 ellipse
 ellipse-mode
 fill
 green
 image
 key
 key-pressed
 key-released
 line
 load-image
 mouse-x
 mouse-y
 mouse-button
 mouse-pressed
 no-fill
 no-stroke
 pmouse-x
 pmouse-y
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
 
 ;; Math
 abs
 ceil
 constrain
 dist
 floor
 lerp
 mag
 remap ; called map in P
 norm
 pow
 radians
 (rename-out [new-random random])
 sq
 sqr

 ;; Time and Date
 year month day hour minute second
 millis

 ;; Transform
 rotate
 scale
 translate
 push-matrix
 pop-matrix

 ;; Conversion
 binary

 ;; Environment / Gui
 cursor
 delay
 display-density
 focused
 frame-count
 frame-rate
 height
 no-cursor
 ; todo: no-smooth
 ; todo: smooth
 pixel-density
 pixel-height
 pixel-width
 set-title
 size
 width
 
 start
 )

;;;
;;; "System Variables"
;;;

(require (for-syntax racket/base))
(define-syntax (width _)         #'(current-width))
(define-syntax (height _)        #'(current-height))
(define-syntax (focused _)       #'(focused?))
(define-syntax (frame-count _)   #'(current-frame-count))
(define-syntax (mouse-x _)       #'(current-mouse-x))
(define-syntax (mouse-y _)       #'(current-mouse-y))
(define-syntax (pmouse-x _)      #'(current-pmouse-x))
(define-syntax (pmouse-y _)      #'(current-pmouse-y))
(define-syntax (mouse-button _)  #'(current-mouse-button))
(define-syntax (mouse-pressed _) #'(current-mouse-pressed))
(define-syntax (key _)           #'(current-key))
(define-syntax (key-pressed _)   #'(current-key-pressed))
(define-syntax (key-released _)  #'(current-key-released))

(define-syntax (frame-rate stx)
  (syntax-case stx ()
    [(_frame-rate fps)     #'(set-frame-rate fps)]
    [id (identifier? #'id) #'(actual-frame-rate)]
    [_ (error 'frame-rate "")]))


(define (start)
  (current-density (display-density))
  (start-gui))
