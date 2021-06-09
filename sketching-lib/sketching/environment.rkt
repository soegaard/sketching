#lang racket/base
(require
 racket/gui/base
 "parameters.rkt")

;;;
;;; Environment
;;;

(provide actual-frame-rate
         ; cursor ; see "gui.rkt"
         delay
         display-density
         set-frame-rate         
         pixel-density
         size)


; set the size of the canvas
; https://processing.org/reference/size_.html
; - in P must be called as the first line of the setup function
; - must only be called once
; - in P cannot be used for resized
(define (size width height [renderer #f])
  ; Note: In P this sets variables. We use parameters.
  ; Todo: Future plans: support multiple renderers
  (current-width  width)
  (current-height height))

; sleep for nap-time milliseconds
(define (delay nap-time)
  (sleep (/ nap-time 1000.)))

; display density
; - returns 2 on a high density screen and 1 otherwise
(define (display-density [monitor #f])
  (inexact->exact
   (if monitor
       (get-display-backing-scale #:monitor monitor)
       (get-display-backing-scale))))

(define (pixel-density density)
  (current-density density)
  (set-pixel-width!  (* density (current-width)))
  (set-pixel-height! (* density (current-height))))


(define (set-frame-rate fps)
  ; https://processing.org/reference/frameRate_.html
  (current-frame-rate fps))

(define (actual-frame-rate)
  (current-actual-frame-rate))
