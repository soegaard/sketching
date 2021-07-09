#lang racket/base
(require
 racket/gui/base
 "environment-no-gui.rkt"
 "parameters.rkt")

;;;
;;; Environment
;;;

(provide actual-frame-rate
         ; cursor ; see "gui.rkt"
         nap
         display-density
         set-frame-rate         
         pixel-density
         size)



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
