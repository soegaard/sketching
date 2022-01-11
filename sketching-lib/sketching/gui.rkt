#lang racket/base
;;;
;;; GUI
;;;

; This file and environment.rkt are the only files that depend on racket/gui.

(provide
 cursor no-cursor
 focused?
 fullscreen
 loop no-loop no-gui
 initialize-gui
 start-gui
 set-title

 load-pixels
 update-pixels
 set-pixel

 save
 )

;;;
;;; Module Imports
;;;

(require racket/gui 
         "parameters.rkt")

;;;
;;; Globals
;;;

(define top-frame     #f)
(define top-canvas    #f)
(define top-bitmap    #f)
(define top-bitmap-dc #f)
(define top-dc        #f)
(define top-timer     #f)


;;;
;;; File
;;;

; Drawing goes to the top-bitmap, which the gui will then copy to screen.
; This allows both `save` and `get-pixel`.
; Drawing directly to the screen would be faster.

(define bitmap-kind
  (list 'unknown/mask 'unknown/alpha
        'gif 'gif/mask 'gif/alpha
        'jpeg 'jpeg/alpha
        'png 'png/mask 'png/alpha
        'xbm 'xbm/alpha 'xpm 'xpm/alpha
        'bmp 'bmp/alpha))

(define (save filepath
              #:kind          [kind          'png]
              #:monochrome?   [monochrome?   #f]
              #:alpha?        [alpha?        #f]
              #:backing-scale [backing-scale 1.0]
              #:quality       [quality       75]) ; default for save-file 
  (unless (member kind bitmap-kind)
    (raise-argument-error
     'save (~a "one of the symbols " (add-between (map ~a bitmap-kind) " "))
     kind))
  (when dc
    (define w     (current-width))
    (define h     (current-height))
    (define bm    (make-object bitmap% w h monochrome? alpha? backing-scale))
    (define bm-dc (new bitmap-dc% [bitmap bm]))
    (send bm-dc draw-bitmap top-bitmap 0 0)
    (send bm save-file filepath kind quality)))


;;;
;;; Event Types
;;;

(define ups   '(left-up   middle-up   right-up))
(define downs '(left-down middle-down right-down))
(define moves '(enter leave motion))

;;;
;;; Frame
;;;

; When our application window is closed, we stop the timer.

(define sketching-frame%
  (class frame%
    (define/augment (on-close)
      (when top-timer
        (send top-timer stop)))
    (super-new)))

;;;
;;; Keys
;;;

; The keyboard state is kept in a hash table. 
; Use key-down? to find out, whether a key is pressed or not.
(define the-keyboard  (make-hasheq))

(define (key-down! k) (hash-set! the-keyboard k #t))
(define (key-up!   k) (hash-set! the-keyboard k #f))
(define (key-down? k) (hash-ref  the-keyboard k #f))
(define (key-up?   k) (not (key-down? k)))


;;;
;;; Canvas
;;;

(define sketching-canvas%
  (class canvas%
    ; (define (on-initial-resize w h) (send this clear))
    (super-new)
    
    (define/override (on-event e) ; mouse events
      (when (is-a? e mouse-event%)
        (current-mouse-x (send e get-x))
        (current-mouse-y (send e get-y)))
      
      (define (released? t) (member t ups))
      (define (pressed?  t) (member t downs))
      (define (moved?    t) (and (member t moves)
                                 (not (current-mouse-left-pressed))
                                 (not (current-mouse-middle-pressed))
                                 (not (current-mouse-right-pressed))))
      (define (dragged?  t) (and (member t moves)
                                 (or (current-mouse-left-pressed)
                                     (current-mouse-middle-pressed)
                                     (current-mouse-right-pressed))))      
      (define type (send e get-event-type))
      (case type
        [(left-up)     (current-mouse-left-pressed   #f) (current-mouse-button 'left)]
        [(middle-up)   (current-mouse-middle-pressed #f) (current-mouse-button 'middle)]
        [(right-up)    (current-mouse-right-pressed  #f) (current-mouse-button 'right)]
        [(left-down)   (current-mouse-left-pressed   #t) (current-mouse-button 'left)]
        [(middle-down) (current-mouse-middle-pressed #t) (current-mouse-button 'middle)]
        [(right-down)  (current-mouse-right-pressed  #t) (current-mouse-button 'right)]
        #;[(enter) ...]
        #;[(leave) ...]
        #;[(motion) ...])
      (define dc (send this get-dc))
      (send this suspend-flush)
      (when (released? type)
        (current-mouse-released #t)
        (current-mouse-pressed  #f)
        (current-mouse-moved    #f)
        (current-mouse-dragged  #f)
        (define on-released (current-on-mouse-released))
        (when on-released (on-released)))
      (when (pressed? type)
        (current-mouse-pressed  #t)
        (current-mouse-released #f)
        (current-mouse-moved    #f)
        (current-mouse-dragged  #f)
        (define on-pressed (current-on-mouse-pressed))
        (when on-pressed (on-pressed)))
      (when (moved? type)
        (current-mouse-moved    #t)
        (current-mouse-dragged  #f)
        (define on-moved (current-on-mouse-moved))
        (when on-moved (on-moved)))
      (when (dragged? type)
        (current-mouse-moved    #f)
        (current-mouse-dragged  #t)
        (define on-dragged (current-on-mouse-dragged))
        (when on-dragged (on-dragged)))
      (send this resume-flush))
    
    (define/override (on-char e)  ; key event
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (send this suspend-flush)
      (cond
        ; key down?
        [(eq? release 'press)  (unless (key-down? key)   ; already down, i.e. this is a repetition                                 
                                 (key-down! key)
                                 (current-key-pressed  #t)
                                 (current-key-released #f)
                                 (current-key key)
                                 (define on-pressed (current-on-key-pressed))
                                 (when on-pressed (on-pressed)))]
        ; key up?
        [else                  (when (key-down? release)
                                 (key-up! release)
                                 (current-key-pressed  #f)
                                 (current-key-released #t)
                                 (current-key release)
                                 (define on-released (current-on-key-released))        
                                 (when on-released (on-released)))])
      (send this resume-flush))

    (define paint-mode 'fast) ; 
    (define/override (on-paint)   ; repaint (exposed or resized)
      (case paint-mode
        [(slow) ; supports load-pixels, save-pixels and friends
         ; 1. Draw off screen to the top-bitmap
         (handle-on-paint top-bitmap-dc)
         ; 2. Now copy the bitmap to the screen
         (define dc (send this get-dc))
         (send this suspend-flush)
         (send dc set-transformation (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
         (send dc draw-bitmap top-bitmap 0 0)
         (send this resume-flush)]
        [(fast)
         (send this suspend-flush)
         (handle-on-paint (send this get-dc))
         (send this resume-flush)]))))

(define (initialize-gui)
  ; Initialize gui before calling `setup`.
  ; The drawing context is now available in `setup`.
  (reset-milliseconds-at-start-of-program! (current-milliseconds)) 

  (define-values (w h) (get-display-size))
  (define frame  (new sketching-frame%
                      [label "sketch"]
                      [style '(fullscreen-button)]                      
                      ; Note [Issue #21]:
                      ;   On macOS [x w] [y 0] will place the frame in the upper right corner.
                      ;   On Windows it will place the frame out of sight. 
                      ;   For now we don't specifify x and y which uses the default placement.
                      ;   On macOS it the upper, left corner of the screen.
                      ))
  (set! top-frame frame)
  
  (define canvas (new sketching-canvas%
                      ; [style '(gl no-autoclear)]
                      ; [style '(no-autoclear)]
                      [parent     frame]
                      [min-width  100]
                      [min-height 100]))
  
  (set! top-canvas            canvas)
  (set! top-bitmap            (send canvas make-bitmap 100 100))
  (set! top-bitmap-dc         (new bitmap-dc% [bitmap top-bitmap]))

  (define b  (new brush% [color "white"])) ; solid white
  (define dc (send canvas get-dc))
  (send dc set-smoothing 'aligned)
  (send dc set-brush b)
  
  (current-dc dc))

(define (frame-rate->timer-interval fps)
  (inexact->exact (floor (/ 1000 fps ))))
  
(define (start-gui)
  ; todo  (current-density (display-density))
  ; store the time now, used by millis
  (send top-canvas min-width  (current-width))
  (send top-canvas min-height (current-height))
  (define old-dc (send top-canvas get-dc))
  (unless (and (= (current-width)  (send top-bitmap get-width))
               (= (current-height) (send top-bitmap get-height)))
    ; (set! top-bitmap    (make-screen-bitmap (current-width) (current-height)))
    (set! top-bitmap    (send top-canvas  make-bitmap (current-width) (current-height)))    
    (set! top-bitmap-dc (new bitmap-dc% [bitmap top-bitmap]))
    ; Now transfer any settings already made.
    ;   - first we transfer the background
    ;(define old (send top-bitmap-dc get-transformation))
    ;(send top-bitmap-dc set-transformation (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
    (send top-bitmap-dc set-background (send old-dc get-background))
    (send top-bitmap-dc clear)
    ;(send top-bitmap-dc set-transformation old)

    (send top-bitmap-dc set-pen   (send old-dc get-pen))
    (send top-bitmap-dc set-brush (send old-dc get-brush))
    (send top-bitmap-dc set-font  (send old-dc get-font))

    (send top-bitmap-dc set-smoothing  (send old-dc get-smoothing))
    )
               
  (send top-canvas set-canvas-background (send (current-dc) get-background))
  (send (send top-canvas get-dc) clear)
  

  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval (frame-rate->timer-interval (current-frame-rate))])) ; milliseconds
  (set! top-timer timer)

  (unless (current-no-gui)
    (send top-frame show #t)))

(define (no-gui)
  (current-no-gui #t))


; returns true if our sketch has focus
(define (focused?)
  (and (send top-frame get-focus-window) #t))

(define (handle-on-paint dc) ; receives top-bitmap-dc
  ; call draw here
  (parameterize ([current-dc dc])
    (define draw (current-draw))
    (when draw
      ; calculate time between frames
      (define previous-milliseconds-at-start-of-frame milliseconds-at-start-of-frame)
      (define now (current-milliseconds))
      (reset-milliseconds-at-start-of-frame! now)    
      (reset-delta-time!
       (if (= previous-milliseconds-at-start-of-frame 0)
           1
           (- now previous-milliseconds-at-start-of-frame)))
      
      ; store old mouse coordinates
      (define old-mouse-x (current-mouse-x))
      (define old-mouse-y (current-mouse-y))    
      ; reset transformations before calling draw
      (define old (send dc get-transformation))
      (send dc set-transformation
            (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
      (draw)
      (send dc set-transformation old)
      ; store previous mouse position
      (current-pmouse-x old-mouse-x)
      (current-pmouse-y old-mouse-y))
    ; increment frame counter
    (current-frame-count (+ 1 (current-frame-count)))
    (void)))


(define timer-stats  (vector 1000. 1000. 1000. 1000. 1000.)) ; milliseconds
(define timer-stats-index 0)
(define timer-previous-time (current-inexact-milliseconds))
(define (timer-stats-mean) (* 0.2 (for/sum ([t timer-stats]) t)))
(define (timer-stat!)
  (define now (current-inexact-milliseconds))
  (define t (- now timer-previous-time))
  (set! timer-previous-time now)
  (vector-set! timer-stats timer-stats-index t)
  (set! timer-stats-index (modulo (+ timer-stats-index 1) 5)))


(define timer-stats  (vector 1000. 1000. 1000. 1000. 1000.)) ; milliseconds
(define timer-stats-index 0)
(define timer-previous-time (current-inexact-milliseconds))
(define (timer-stats-mean) (* 0.2 (for/sum ([t timer-stats]) t)))
(define (timer-stat!)
  (define now (current-inexact-milliseconds))
  (define t (- now timer-previous-time))
  (set! timer-previous-time now)
  (vector-set! timer-stats timer-stats-index t)
  (set! timer-stats-index (modulo (+ timer-stats-index 1) 5)))

(define (handle-on-timer)
  ; Note: Beware that (send top-timer interval) returns the number
  ;       that the timer was set to, not the actual time.
  (timer-stat!)  
  (current-actual-frame-rate (/ (round (* 10 (/ 1000 (timer-stats-mean)))) 10))
  (when (current-loop-running?)
    (send top-canvas on-paint) ;xxx
    ; change timer interval, if frame-rate has been set
    (define current-interval (frame-rate->timer-interval (current-frame-rate)))
    (unless (equal? (send top-timer interval) current-interval)
      (send top-timer start current-interval))))


(define cursor-symbols '(arrow bullseye cross hand ibeam watch blank
                               size-n/s size-e/w size-ne/sw size-nw/se))


(define (cursor symbol-or-image)
  (define s symbol-or-image)
  (cond
    [(symbol? s)
     (when (member s cursor-symbols)
       (define c (make-object cursor% s))
       (send top-frame set-cursor c))]
    [else (error 'cursor "todo: implement support for image cursors")]))

(define the-blank-cursor (make-object cursor% 'blank))

; TODO: neither solution works on Big Sur, so ... the problem might be elsewhere.
(define (no-cursor)
  (send top-frame set-cursor #f))
#;(define (no-cursor)
  (define c (send top-frame get-cursor))
  (unless (eq? c the-blank-cursor)
    (send top-frame set-cursor the-blank-cursor)))


(define (set-title s)
  (when (label-string? s)
    (send top-frame set-label s)))

(define (fullscreen)
  (when top-frame
    (send top-frame fullscreen #t)
    (define-values (w h) (get-display-size))
    (current-width w)
    (current-height h)))


(define (loop)
  (current-loop-running? #t))

(define (no-loop)
  (current-loop-running? #f))


(define (color->bytes c)
  (define a (exact-floor (* 255 (send c alpha))))
  (define r (send c red))
  (define g (send c green))
  (define b (send c blue))
  (bytes a r g b))


; An attempt to let `load-image` create canvas bitmaps
; instead of regular bitmaps. Didn't work out at first try.

;; (define (bitmap->canvas-bitmap bm)
;;   (define w   (send bm get-width))
;;   (define h   (send bm get-height))
;;   (define cbm (send top-canvas make-bitmap w h))
;;   ; (define cbm (make-screen-bitmap w h))
;;   (define cdc (new bitmap-dc% [bitmap cbm]))
;;   (send cdc draw-bitmap bm 0 0)
;;   cbm)

;; (current-bitmap->canvas-bitmap bitmap->canvas-bitmap)


;;; ---------------------

;;;
;;; load-pixels, set-pixel and update-pixels
;;; (using a four byte strings for each channel)
;;;


(define argb-pixels-width  0)
(define argb-pixels-height 0)

(define argb-as            (bytes))
(define argb-rs            (bytes))
(define argb-gs            (bytes))
(define argb-bs            (bytes))

(define (load-pixels)
  ; TODO copy the contents of the canvas to the bitmap first
  ; TODO - is it possible to copy the canvas to a bitmap?
  (define w  (send top-canvas get-width))
  (define h  (send top-canvas get-height))
  ; make a bitmap that draws the same way as the canvas
  ; (logical units vs pixels ...)
  ; TODO (define bm (send top-canvas make-bitmap w h))
  ; TODO (define bs (make-bytes (* w h 4) 0))
  ; TODO (send bm get-argb-pixels 0 0 w h bs)
  (set! argb-pixels-width  w)
  (set! argb-pixels-height h)
  (set! argb-as (make-bytes (* w h) 0))
  (set! argb-rs (make-bytes (* w h) 0))
  (set! argb-gs (make-bytes (* w h) 0))
  (set! argb-bs (make-bytes (* w h) 0)))


(define (set-pixel x y col)
  (define i (+ (* argb-pixels-width y) x))
  (bytes-set! argb-as i (exact-floor (* 255. (send col alpha))))
  (bytes-set! argb-rs i (send col red))
  (bytes-set! argb-gs i (send col green))
  (bytes-set! argb-bs i (send col blue)))

(define (update-pixels)
  ; Copy the pixels to the canvas:
  ;   1. Make bitmap
  ;   2. Convert pixels (vector of argb bytes to single bytes)
  ;   3. Draw result on bitmap
  ;   4. Draw bitmap on canvas
  (define w  argb-pixels-width)
  (define h  argb-pixels-height)
  ; make a bitmap that draws the same way as the canvas
  ;   (logical units vs pixels ...)

  ;; 1. Make screen bitmap 
  ; (define bm (send top-canvas make-bitmap w h))
  ; Nope: make normal bitmap (screen bitmaps don't support set-argb-pixels)
  (define bm    (make-bitmap w h #t #:backing-scale 1.0))
  ;; 2. Convert pixels to argb byte string
  (define bs (make-bytes (* w h 4) 0))
  (for ([i (in-range 0 (* w h) 4)])
    (bytes-set! bs    i    (bytes-ref argb-as i))
    (bytes-set! bs (+ i 1) (bytes-ref argb-rs i))
    (bytes-set! bs (+ i 2) (bytes-ref argb-gs i))
    (bytes-set! bs (+ i 4) (bytes-ref argb-bs i)))
  ;; 3. Draw result on bitmap
  (send bm set-argb-pixels 0 0 w h bs)
  ;; 4. Draw bitmap on canvas
  (define dc (send top-canvas get-dc))
  ; (displayln (list 'backing-scale (send dc get-backing-scale)))
  (send dc set-scale 4. 4.)  ; why 4 here?  (I expected 2. 2.)
  (send dc draw-bitmap bm 0 0))


;;; ---------------------

;;;
;;; load-pixels, set-pixel and update-pixels
;;; (using a bitmap)
;;;

;; (define loaded-pixels-width  0)
;; (define loaded-pixels-height 0)
;; (define loaded-pixels-bitmap #f)
;; (define loaded-pixels-dc     #f)


;; (define (load-pixels)
;;   ; TODO copy the contents of the canvas to the bitmap first
;;   ; TODO - is it possible to copy the canvas to a bitmap
;;   (define w  (send top-canvas get-width))
;;   (define h  (send top-canvas get-height))
;;   ; make a bitmap that draws the same way as the canvas
;;   ; (logical units vs pixels ...)
;;   ; (define bm (send top-canvas make-bitmap w h))
;;   (define bm (make-bitmap w h))
;;   (define dc (new bitmap-dc% [bitmap bm]))
;;   (set! loaded-pixels-width  w)
;;   (set! loaded-pixels-height h)
;;   (set! loaded-pixels-bitmap bm)
;;   (set! loaded-pixels-dc dc))

;; (define (set-pixel x y col)
;;   (send loaded-pixels-dc set-pixel x y col))

;; (define (update-pixels)
;;   (define dc (send top-canvas get-dc))
;;   (send dc draw-bitmap loaded-pixels-bitmap 0 0))


;;; ---------------------


;;;
;;; load-pixels, set-pixel and update-pixels
;;; (using a vector of argb-bytes)
;;;

;; (define argb-pixels-width  0)
;; (define argb-pixels-height 0)
;; (define argb-pixels        #f)

;; (define (load-pixels)
;;   ; TODO copy the contents of the canvas to the bitmap first
;;   ; TODO - is it possible to copy the canvas to a bitmap
;;   (define w  (send top-canvas get-width))
;;   (define h  (send top-canvas get-height))
;;   ; make a bitmap that draws the same way as the canvas
;;   ; (logical units vs pixels ...)
;;   ; TODO (define bm (send top-canvas make-bitmap w h))
;;   ; TODO (define bs (make-bytes (* w h 4) 0))
;;   ; TODO (send bm get-argb-pixels 0 0 w h bs)
;;   (set! argb-pixels (make-vector (* w h) (bytes 0 0 0 255)))
;;   (set-pixels! argb-pixels) ; vector og bytes
;;   (set! argb-pixels-width  w)
;;   (set! argb-pixels-height h))


;; (define (set-pixel x y col)
;;   (define w argb-pixels-width)
;;   ; (define h argb-pixels-height)
;;   (define b (color->bytes col))
;;   (vector-set! argb-pixels (+ (* w y) x) b))

;; (define (update-pixels)
;;   ; Copy the pixels to the canvas:
;;   ;   1. Make screen bitmap
;;   ;   2. Convert pixels (vector of argb bytes to single bytes)
;;   ;   3. Draw result on bitmap
;;   ;   4. Draw bitmap on canvas
;;   (define w  argb-pixels-width)
;;   (define h  argb-pixels-height)
;;   ; make a bitmap that draws the same way as the canvas
;;   ;   (logical units vs pixels ...)

;;   ;; 1. Make screen bitmap 
;;   ; (define bm (send top-canvas make-bitmap w h))
;;   ; Nope: make normal bitmap (screen bitmaps don't support set-argb-pixels)
;;   (define bm    (make-bitmap w h))
;;   ;; 2. Convert pixels to argb byte string
;;   (define bs (make-bytes (* w h 4) 0))
;;   (define ps pixels)
;;   (for* ([y (in-range h)]
;;          [x (in-range w)])
;;     (define i (+ (* w y) x))
;;     (bytes-copy! bs (* 4 i) (vector-ref ps i)))
;;   ;; 3. Draw result on bitmap
;;   (send bm set-argb-pixels 0 0 w h bs)
;;   ;; 4. Draw bitmap on canvas
;;   (define dc (send top-canvas get-dc))
;;   (send dc draw-bitmap bm 0 0))

  
