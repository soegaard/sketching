#lang racket/base
; This file contains dummy definitions for documentation mode.

(require (for-syntax syntax/parse racket/base))

(define-syntax (define-dummies stx)
  (syntax-parse stx
    [(_define-dummies id ...)
     (syntax/loc stx
       (begin
         (begin
           (define (id) (void))
           (provide id))
         ...))]))

(define-dummies
  ; no-gui
  display-density
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
  
  initialize
  start
  
  frame-rate
  set-frame-rate!

  nap
  save
  
  on-mouse-pressed
  on-mouse-dragged
  on-mouse-moved
  on-mouse-released
  on-key-pressed
  on-key-released
  on-key-typed
  on-resize
  )
