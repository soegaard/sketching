#lang racket/base
(require "parameters.rkt")
(provide size nap)

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
(define (nap nap-time)
  (sleep (/ nap-time 1000.)))
