#lang racket/base
(provide
 cursor no-cursor
 focused?
 start-gui
 set-title)

(require racket/gui
         "parameters.rkt")

(define top-frame  #f)
(define top-canvas #f)
(define top-timer  #f)

(define sketching-frame%
  (class frame%
    (define/augment (on-close)
      (when top-timer
        (send top-timer stop)))
    (super-new)))


(define ups   '(left-up   middle-up   right-up))
(define downs '(left-down middle-down right-down))
(define moves '(enter leave motion))

(define sketching-canvas%
  (class canvas%
    (define/override (on-event e) ; mouse events
      (when (is-a? e mouse-event%)
        (current-mouse-x (send e get-x))
        (current-mouse-y (send e get-y))
        ;(displayln (list 'new (current-mouse-x) (current-mouse-y)))
        )
      
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
      (send this resume-flush)
      )
    
    (define/override (on-char e)  ; key event
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (send this suspend-flush)
      (when (eq? release 'press)  ; key down?
        (current-key-pressed  #t)
        (current-key-released #f)
        (current-key key)
        (define on-pressed (current-on-key-pressed))        
        (when on-pressed (on-pressed)))
      (when (eq? key 'release)    ; key up?
        (current-key-pressed  #f)
        (current-key-released #t)
        (current-key key)
        (define on-released (current-on-key-released))        
        (when on-released (on-released)))
      (send this resume-flush))
        
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (handle-on-paint dc)
      (send this resume-flush))
    (super-new)))


(define (start-gui)
  ; store the time now, used by millis
  (reset-milliseconds-at-start-of-program! (current-milliseconds)) 
  
  (define frame  (new sketching-frame%
                      [label "sketch"]))
  (set! top-frame frame)
  
  (define canvas (new sketching-canvas%
                      [parent       frame]
                      [min-width  (current-width)]
                      [min-height (current-height)]))
  (set! top-canvas canvas)

  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval (inexact->exact (floor (/ 1000 (current-frame-rate))))])) ; milliseconds
  (set! top-timer timer)
  
  (send frame show #t))




; returns true if our sketch has focus
(define (focused?)
  (and (send top-frame get-focus-window) #t))

(define (handle-on-paint dc)
  ; call draw here
  (current-dc dc)
  (define draw (current-draw))
  (when draw
    ; store old mouse coordinates
    ; (define old-mouse-x (current-mouse-x))
    ; (define old-mouse-y (current-mouse-y))    
    ; reset transformations before calling draw
    (define old (send dc get-transformation))
    (send dc set-transformation
          (vector (vector 1 0 0 1 0 0) 0 0 1 1 0))
    ; (displayln (list 'old (current-pmouse-x) (current-pmouse-y)))

    (draw)
    (send dc set-transformation old)
      ; store previous mouse position
    ; (current-pmouse-x old-mouse-x)
    ; (current-pmouse-y old-mouse-y))
    (current-pmouse-x (current-mouse-x))
    (current-pmouse-y (current-mouse-y)))
  ; increment frame counter
  (current-frame-count (+ 1 (current-frame-count)))
  (void))

(define (handle-on-timer)
  (send top-canvas on-paint))

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

(define (no-cursor)
  (send top-frame set-cursor #f))

(define (set-title s)
  (when (label-string? s)
    (send top-frame set-label s)))

