#lang racket/base
; This contains dummy definition for documentation mode.

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
  )




(provide (rename-out [sketching-module-begin #%module-begin]))
(require "parameters.rkt")

(define-syntax (sketching-module-begin stx)
  (syntax-parse stx
    [(_sketching-module-begin def/expr ...)
     (define ctx (car (syntax->list #'(def/expr ...))))
     (with-syntax (;[initialize        (datum->syntax ctx 'initialize)]
                   [setup             (datum->syntax ctx 'setup)]
                   [draw              (datum->syntax ctx 'draw)]
                   [on-mouse-pressed  (datum->syntax ctx 'on-mouse-pressed)]
                   [on-mouse-released (datum->syntax ctx 'on-mouse-released)]
                   [on-mouse-moved    (datum->syntax ctx 'on-mouse-moved)]
                   [on-mouse-dragged  (datum->syntax ctx 'on-mouse-dragged)]
                   [on-key-pressed    (datum->syntax ctx 'on-key-pressed)]
                   [on-key-released   (datum->syntax ctx 'on-key-released)]
                   [default-setup     (datum->syntax ctx 'default-setup)]
                   [default-draw      (datum->syntax ctx 'default-draw)])
     (syntax/loc stx
       (#%module-begin
        ; (initialize) ; setup frame, canvas and drawing context (pen, brush)
        def/expr ...
        (define (default-setup) (void))
        (define (default-draw)  (void))        
        (setup)
        (current-draw draw)
        (current-on-mouse-pressed  on-mouse-pressed)
        (current-on-mouse-released on-mouse-released)
        (current-on-mouse-moved    on-mouse-moved)
        (current-on-mouse-dragged  on-mouse-dragged)
        (current-on-key-pressed    on-key-pressed)
        (current-on-key-released   on-key-released)
        ; (start) ; start event loop
        )))]))
