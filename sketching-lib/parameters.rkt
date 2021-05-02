#lang racket/base
(provide
 current-actual-frame-rate ; for the system variable frame-rate
 current-density
 current-draw              
 current-dc  dc              ; set by on-paint before calling draw
 current-ellipse-mode
 current-rect-mode
 current-frame-count
 current-frame-rate
 current-height
 current-image-mode
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

 current-key
 current-key-pressed
 current-key-released
 current-on-key-pressed
 current-on-key-released
 
 current-width

 milliseconds-at-start-of-program ; variable
 reset-milliseconds-at-start-of-program!
 
 pixel-height set-pixel-height!
 pixel-width  set-pixel-width!)

(define (positive-integer? x)
  (and (number? x) (positive? x) (integer? x) x))

(define (non-negative-integer? x)
  (and (number? x) (integer? x) (or (zero? x) (positive? x)) x))

(define (make-at-least-guard n)
  (λ (x) (and (positive-integer? n) (>= x n) x)))

(define (make-one-of-guard options)
  (λ (x) (and (member x options) x)))

; Size of the canvas
(define current-width  (make-parameter 100 (make-at-least-guard 100)))
(define current-height (make-parameter 100 (make-at-least-guard 100)))

; Pixel density
(define current-density (make-parameter 1 (make-one-of-guard '(1 2)))) 
(define pixel-width     1) 
(define pixel-height    1)

(define (set-pixel-width!  n) (set! pixel-width  n))
(define (set-pixel-height! n) (set! pixel-height n))

; Frames
(define current-frame-count        (make-parameter  0 non-negative-integer?))
(define current-frame-rate         (make-parameter 60 positive-integer?))
(define current-actual-frame-rate  (make-parameter 10 positive-integer?))

(define current-draw (make-parameter #f))
(define dc           #f)
(define current-dc   (make-parameter #f (λ (x) (set! dc x) x)))

(define current-ellipse-mode (make-parameter 'center (make-one-of-guard '(center radius corner corners))))
(define current-rect-mode    (make-parameter 'corner (make-one-of-guard '(center radius corner corners))))


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

(define current-image-mode           (make-parameter 'center))

(define milliseconds-at-start-of-program 0) ; set by start
(define (reset-milliseconds-at-start-of-program! n)
  (set! milliseconds-at-start-of-program n))
