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

(require racket/format racket/list)

;;; Predicates

(define (positive-integer? x)
  (and (number? x) (positive? x) (integer? x) x))

(define (non-negative-integer? x)
  (and (number? x) (integer? x) (or (zero? x) (positive? x)) x))

(define (real-number? x)
  (and (number? x) (real? x) x))

(define (one-of? x options)
  (member x options))

(define (string-or-false? x)
  (or (string? x) (eq? x #f)))


;;; Guards

(define (make-guard who arg-name check? message [default #f])
  (λ (x)
    (cond
      [(check? x) x]
      [default    default]
      [else       (raise-arguments-error who message arg-name x)])))

(define (make-positive-integer-guard who arg-name)  
  (make-guard who arg-name positive-integer? "positive integer (at least 1)"))

(define (make-non-negative-integer-guard who arg-name)  
  (make-guard who arg-name non-negative-integer? "non-negative integer (at least 0)"))

(define (make-real-guard who arg-name)
  (make-guard who arg-name real-number? "real number"))

(define (make-one-of-guard who arg-name options)
  (define (check? x) (one-of? x options))
  (define message    (apply ~a "one of these were expected: " (add-between (map ~a options) ", ")))
  (make-guard who arg-name check? message))

(define (make-boolean-guard who arg-name)
  (make-guard who arg-name boolean? "boolean"))

(define (make-string-guard who arg-name)
  (make-guard who arg-name string? "string"))

(define (make-string-or-false-guard who arg-name)
  (make-guard who arg-name string-or-false? "string or #f"))


; Note: The width-guard and height-guard only checks
;       that width and height are positive integers.
;       The smallest size of a gui window is 100x100,
;       but other drawing contexts (say an svg) allow
;       smaller size. Instead we make "gui.rkt" open a
;       larger window, if the sizes are too small.

(define width-guard  (make-positive-integer-guard 'width  "width"))
(define height-guard (make-positive-integer-guard 'height "height"))


;; (define (make-color/false-guard who)
;;   (λ (x)
;;     (cond
;;       [(eq? x #f)   #f]
;;       [(integer? x) x]
;;       [(color? x)   x]
;;       [else         (raise-argument-error who "a color or #f" x)])))


; Size of the canvas
(define current-width  (make-parameter 100 width-guard))
(define current-height (make-parameter 100 height-guard))

; Pixel density
(define current-density (make-parameter 1 (make-one-of-guard 'density "density" '(1 2))))
(define pixel-width     1) 
(define pixel-height    1)

(define (set-pixel-width!  n) (set! pixel-width  n))
(define (set-pixel-height! n) (set! pixel-height n))

; Frames
(define current-frame-count        (make-parameter  0 (make-non-negative-integer-guard 'frame-count       "n")))
(define current-frame-rate         (make-parameter 15 (make-positive-integer-guard     'frame-rate        "n")))
(define current-actual-frame-rate  (make-parameter 10 (make-real-guard                 'actual-frame-rate "r")))

(define current-draw    (make-parameter #f))
(define dc              #f)
(define current-dc      (make-parameter #f (λ (x) (set! dc x) x)))
(define current-no-gui  (make-parameter #f)) ; #t = no gui, #f = show gui
(define current-fill    (make-parameter #f))

(define current-ellipse-mode (make-parameter 'center (make-one-of-guard 'ellipse-mode "mode" '(center radius corner corners))))
(define current-rect-mode    (make-parameter 'corner (make-one-of-guard 'rect-mode    "mode" '(center radius corner corners))))
(define current-color-mode   (make-parameter 'rgb    (make-one-of-guard 'color-mode   "mode" '(rgb hsb))))
(define current-image-mode   (make-parameter 'center (make-one-of-guard 'image-mode   "mode" '(center radius corner corners))))
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
  


(define current-font-size            (make-parameter 12       (λ (x) (or (and (<= 0.0 x 1024.0) x) 12))))
(define current-font-face            (make-parameter "Monaco" (make-string-or-false-guard 'font-face "face")))
(define current-font-style           (make-parameter 'normal  (make-one-of-guard 'font-style "style" '(normal slant italic))))
(define current-font-weight          (make-parameter 'normal  (λ (x) (or (and (or (and (integer? x) (<= 100 x 1000))
                                                                                  (member x '(thin ultralight light semilight
                                                                                                   book normal medium semibol bold
                                                                                                   ultrabold heavy ultraheavy)))
                                                                              x)
                                                                         'normal))))
(define current-font-underlined?     (make-parameter #f       (make-boolean-guard 'font-underlined?     "underlined?")))
(define current-font-smoothing       (make-parameter 'default (make-one-of-guard  'font-smoothing       "mode" '(default partly-smoothed smoothed unsmoothed))))
(define current-font-size-in-pixels? (make-parameter #f       (make-boolean-guard 'font-size-in-pixels? "mode"))) ; #f means size in pixels
(define current-font-hinting         (make-parameter 'aligned (make-one-of-guard  'font-hinting         "mode" '(aligned unaligned))))
(define current-font-family          (make-parameter 'default (make-one-of-guard  'font-family          "mode" '(default decorative roman script swiss modern symbol system))))

(define current-text-horizontal-align (make-parameter 'left   (make-one-of-guard 'text-horizontal-align "mode" '(left right center))))
(define current-text-vertical-align   (make-parameter 'top    (make-one-of-guard 'text-vertical-align   "mode" '(top bottom center baseline))))

(define pixels                        (make-parameter #f))
(define (set-pixels! v) (set! pixels v))


(define current-shapes                (make-parameter '()))

(define current-bitmap->canvas-bitmap (make-parameter (λ (x) x)))
