#lang racket/base
(require racket/gui)

; Mouse coordinates previous frame
(define pmouse-x 0)
(define pmouse-y 0)

; Mouse coordinates current frame
(define mouse-x 0)
(define mouse-y 0)

; A frame containing a single canvas with a timer that
; continously calls draw
(define top-frame  #f)
(define top-canvas #f)
(define top-timer  #f)

(define dc #f) ; drawing context of the canvas

(define (draw)
  (when dc
    (send dc draw-line pmouse-x pmouse-y mouse-x mouse-y)))


(define my-frame%
  (class frame%
    (define/augment (on-close)
      (when top-timer
        (send top-timer stop)))
    (super-new)))


(define my-canvas%
  (class canvas%
    (define/override (on-event e) ; mouse events
      (when (is-a? e mouse-event%)
        (set! mouse-x (send e get-x))
        (set! mouse-y (send e get-y))
        (set! dc (send this get-dc))))
        
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (handle-on-paint dc)
      (send this resume-flush))
    (super-new)))


(define (start-gui)
  (define frame  (new my-frame%
                      [label "sketch"]))
  (set! top-frame frame)  
  (define canvas (new my-canvas%
                      [parent     frame]
                      [min-width  400]
                      [min-height 400]))
  (set! top-canvas canvas)

  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval (inexact->exact (floor (/ 1000 30)))])) ; milliseconds
  (set! top-timer timer)
  
  (send frame show #t))

(define (handle-on-paint dc)
  ; call draw here
  (when dc 
    ; store old mouse coordinates
    (define old-mouse-x mouse-x)
    (define old-mouse-y mouse-y)
    (draw)
    (set! pmouse-x old-mouse-x)
    (set! pmouse-y old-mouse-y)))

(define (handle-on-timer)
  (send top-canvas on-paint))

(start-gui)
