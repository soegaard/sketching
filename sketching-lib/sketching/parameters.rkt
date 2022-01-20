#lang racket/base
(provide
 current-color-mode
 current-ellipse-mode
 current-image-mode
 current-rect-mode
 current-tint-color        ; #f or a color
 current-tint-brush        ; a brush%

 current-actual-frame-rate ; for the system variable frame-rate
 current-density
 current-draw              
 current-dc  dc              ; set by on-paint before calling draw
 current-fill

 current-font-size 
 current-font-face
 current-font-family
 current-font-style
 current-font-weight
 current-font-underlined?
 current-font-smoothing
 current-font-size-in-pixels?
 current-font-hinting

 current-text-horizontal-align
 current-text-vertical-align
 
 current-frame-count
 current-frame-rate
 current-height
 current-loop-running?       ; set, unset by loop and no-loop
 current-mouse-x
 current-mouse-y 
 current-pmouse-x
 current-pmouse-y
 current-on-mouse-released 
 current-on-mouse-pressed  
 current-on-mouse-moved    
 current-on-mouse-dragged
 
 current-mouse-released 
 current-mouse-pressed  
 current-mouse-moved    
 current-mouse-dragged
 current-mouse-left-pressed
 current-mouse-middle-pressed
 current-mouse-right-pressed
 current-mouse-button

 current-no-gui
 
 current-key
 current-key-pressed
 current-key-released
 current-on-key-pressed
 current-on-key-released

 current-shapes ; a list based queue of shapes
 
 current-width

 ; variables (not parameters)
 milliseconds-at-start-of-program reset-milliseconds-at-start-of-program! 
 milliseconds-at-start-of-frame reset-milliseconds-at-start-of-frame!
 delta-time reset-delta-time!

 pixels set-pixels!
 
 pixel-height set-pixel-height!
 pixel-width  set-pixel-width!

 current-bitmap->canvas-bitmap
 )

(define (positive-integer? x)
  (and (number? x) (positive? x) (integer? x) x))

(define (non-negative-integer? x)
  (and (number? x) (integer? x) (or (zero? x) (positive? x)) x))

;; This guard is not used anymore.
;; It got split into width-guard and height-guard
;; (define (make-at-least-guard n)
;; ; BUG?: Should this return #f or at least n?
;;   (λ (x) (and (positive-integer? n) (>= x n) x)))

(define (width-guard x)
  (unless (and (positive-integer? x) (>= x 100)) ; raises an error when the width of the canvas is set to below 100 pixels
    (raise-argument-error 'width
                          "width must be greater or equal to 100"
                          x))
  x)

(define (height-guard x)
  (unless (and (positive-integer? x) (>= x 100)) ; raises an error when the height of the canvas is set to below 100 pixels
    (raise-argument-error 'height
                          "height must be greater or equal to 100"
                          x))
  x)

(define (make-one-of-guard options)
  (λ (x) (and (member x options) x)))

(define (real-guard x)
  (and (number? x) (real? x) x))


;; (define (make-color/false-guard who)
;;   (λ (x)
;;     (cond
;;       [(eq? x #f)   #f]
;;       [(integer? x) x]
;;       [(color? x)   x]
;;       [else         (raise-argument-error who "a color or #f" x)])))

(define (boolean-guard x) (and (boolean? x) x))
(define (string-guard  x) (and (string?  x) x))

; Size of the canvas
(define current-width  (make-parameter 100 (width-guard 100)))
(define current-height (make-parameter 100 (height-guard 100)))

; Pixel density
(define current-density (make-parameter 1 (make-one-of-guard '(1 2)))) 
(define pixel-width     1) 
(define pixel-height    1)

(define (set-pixel-width!  n) (set! pixel-width  n))
(define (set-pixel-height! n) (set! pixel-height n))

; Frames
(define current-frame-count        (make-parameter  0 non-negative-integer?))
(define current-frame-rate         (make-parameter 15 positive-integer?))
(define current-actual-frame-rate  (make-parameter 10 real-guard))

(define current-draw (make-parameter #f))
(define dc           #f)
(define current-dc      (make-parameter #f (λ (x) (set! dc x) x)))
(define current-no-gui  (make-parameter #f)) ; #t = no gui, #f = show gui
(define current-fill    (make-parameter #f))

(define current-ellipse-mode (make-parameter 'center (make-one-of-guard '(center radius corner corners))))
(define current-rect-mode    (make-parameter 'corner (make-one-of-guard '(center radius corner corners))))
(define current-color-mode   (make-parameter 'rgb    (make-one-of-guard '(rgb hsb))))
(define current-image-mode   (make-parameter 'center (make-one-of-guard '(center radius corner corners))))
(define current-tint-color   (make-parameter #f      #;(make-color/false-guard 'current-tint-color)))
(define current-tint-brush   (make-parameter #f))    ; a brush%
                                                                

(define current-loop-running?        (make-parameter #t))
(define current-mouse-x              (make-parameter 0))
(define current-mouse-y              (make-parameter 0))
(define current-pmouse-x             (make-parameter 0))  ; previous frame
(define current-pmouse-y             (make-parameter 0))  ; previous frame
(define current-on-mouse-released    (make-parameter #f))
(define current-on-mouse-pressed     (make-parameter #f)) ; handler
(define current-on-mouse-moved       (make-parameter #f))
(define current-on-mouse-dragged     (make-parameter #f))
(define current-mouse-released       (make-parameter #f)) ; bool
(define current-mouse-pressed        (make-parameter #f)) ; bool
(define current-mouse-moved          (make-parameter #f)) ; bool
(define current-mouse-dragged        (make-parameter #f)) ; bool
(define current-mouse-left-pressed   (make-parameter #f))
(define current-mouse-middle-pressed (make-parameter #f))
(define current-mouse-right-pressed  (make-parameter #f))
(define current-mouse-button         (make-parameter #f)) ; Note: P uses 0 for "no button" we use #f

(define current-key                  (make-parameter #f)) ; key
(define current-key-pressed          (make-parameter #f)) ; boolean
(define current-key-released         (make-parameter #f)) ; boolean
(define current-on-key-pressed       (make-parameter #f)) ; call back
(define current-on-key-released      (make-parameter #f)) ; call back


(define milliseconds-at-start-of-program 0) ; set by start
(define (reset-milliseconds-at-start-of-program! n)
  (set! milliseconds-at-start-of-program n))

(define milliseconds-at-start-of-frame 0) ; set by start
(define (reset-milliseconds-at-start-of-frame! n)
  (set! milliseconds-at-start-of-frame n))

(define delta-time 0)
(define (reset-delta-time! n)
  (set! delta-time n))
  


(define current-font-size            (make-parameter 12      (λ (x) (and (<= 0.0 x 1024.0) x))))
(define current-font-face            (make-parameter "Monaco" string-guard))
(define current-font-style           (make-parameter 'normal (make-one-of-guard '(normal slant italic))))
(define current-font-weight          (make-parameter 'normal (λ (x)  (and (or (and (integer? x) (<= 100 x 1000))
                                                                              (member x '(thin ultralight light semilight
                                                                                               book normal medium semibol bold
                                                                                               ultrabold heavy ultraheavy)))
                                                                          x))))
(define current-font-underlined?     (make-parameter #f       boolean-guard))
(define current-font-smoothing       (make-parameter 'default (make-one-of-guard '(default partly-smoothed smoothed unsmoothed))))
(define current-font-size-in-pixels? (make-parameter #f       boolean-guard)) ; #f means size in pixels
(define current-font-hinting         (make-parameter 'aligned (make-one-of-guard '(aligned unaligned))))
(define current-font-family          (make-parameter 'default (make-one-of-guard '(default decorative roman script swiss modern symbol system))))

(define current-text-horizontal-align (make-parameter 'left    (make-one-of-guard '(left right center))))
(define current-text-vertical-align   (make-parameter 'top     (make-one-of-guard '(top bottom center baseline))))

(define pixels                        (make-parameter #f))
(define (set-pixels! v) (set! pixels v))


(define current-shapes                (make-parameter '()))

(define current-bitmap->canvas-bitmap (make-parameter (λ (x) x)))
