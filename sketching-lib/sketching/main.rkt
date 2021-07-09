#lang racket/base
; (require "exports-all.rkt")    (provide (all-from-out "exports-all.rkt"))

(require               "exports-no-gui.rkt" "exports-only-for-docs.rkt")
(provide (all-from-out "exports-no-gui.rkt" "exports-only-for-docs.rkt"))


;; ;;;
;; ;;; Sketching
;; ;;;

;; ;;;
;; ;;; Sketching - as a #lang language
;; ;;;

;; ; The user writes a standard module with expressions and definition in some order. 
;; ;   #lang sketching
;; ;   ...
;; ;   (define (draw) ...)
;; ;   (define (setup) ...)

;; ; Our module-begin turns this into

;; ;   #lang sketching
;; ;   ...
;; ;   (define (draw) ...)
;; ;   (define (setup) ...)
;; ;   (setup) (current-draw draw) (start)

;; ; Now the user can omit draw and setup and in that case we need to use
;; ; a default. We use #%top to catch these.


;; ;;;
;; ;;; Imports
;; ;;;

;; (require (for-syntax syntax/parse racket/base syntax/strip-context racket/string
;;                      racket/syntax)
;;          (except-in racket/class class)
;;          racket/format
;;          "class.rkt"          
;;          "color.rkt"
;;          "conversion.rkt"
;;          "dot-method.rkt"
;;          "environment.rkt"
;;          "gui.rkt"
;;          "graphics.rkt"
;;          "math.rkt"
;;          "math-operators.rkt"
;;          "parameters.rkt"
;;          "time-and-date.rkt"
;;          "transform.rkt")

;; ;;;
;; ;;; #%top - Top-level variable references
;; ;;; 

;; ; SYNTAX  (sketching-top . id)

;; ;   Like #%top, but additionally:
;; ;     - identifiers containing a dot is rewritten to use dot-field,
;; ;       enabling  (#%app foo.bar 1 2) -> (send foo bar 1 2 )
;; ;       and       foo.bar             -> (get-field foo bar)
;; ;     - default values for the event handlers:
;; ;          on-mouse-pressed, on-mouse-released, on-mouse-moved, on-mouse-dragged,
;; ;          on-key-pressed, on-key-released
;; ;     - default values for  setup  and  draw

;; ; Note: sketching-top is exported as #%top.

;; (define-syntax (sketching-top stx)
;;   (syntax-parse stx
;;     [(top . top-id)
;;      (with-syntax ([default-setup (datum->syntax #'top 'default-setup)]
;;                    [default-draw  (datum->syntax #'top 'default-draw)])
;;        (case (syntax->datum #'top-id)
;;          [(setup)             #'default-setup]
;;          [(draw)              #'default-draw]
;;          [(on-mouse-pressed)  #'#f]
;;          [(on-mouse-released) #'#f]
;;          [(on-mouse-moved)    #'#f]
;;          [(on-mouse-dragged)  #'#f]
;;          [(on-key-pressed)    #'#f]
;;          [(on-key-released)   #'#f]
;;          [else
;;           (define str (symbol->string (syntax-e #'top-id)))
;;           (cond
;;             [(string-contains? str ".")
;;              (define ids (map string->symbol (string-split str ".")))
;;              (with-syntax ([(id ...) (for/list ([id ids])
;;                                        (datum->syntax #'top id))])
;;                #'(dot-field id ...))]
;;             [else
;;              #'(#%top . top-id)])]))]))

;; ;;;
;; ;;; #%app - Procedure application
;; ;;; 

;; ; SYNTAX  (sketching-app proc-expr arg ...)

;; ;   Like #%app, but additionally:
;; ;     - a call to  o.m  where o is an object and m is a mtheod name,
;; ;       is rewritten to a method call
;; ;     - a call to  f, where the identifier f contains no dots,
;; ;       is a normal call.

;; (define-syntax (sketching-app stx)
;;   (syntax-parse stx
;;     ; calls to a   o.m is a method call
;;     ; calls to an  f   is a normal call
;;     [(app proc-id:id . args)
;;      ; (display ".")
;;      (define str (symbol->string (syntax-e #'proc-id)))
;;      (cond
;;        [(string-contains? str ".")
;;         (define ids (map string->symbol (string-split str ".")))
;;         (with-syntax ([(id ...) (for/list ([id ids])
;;                                   (datum->syntax #'proc-id id))])
;;           #'(app-dot-method (id ...) . args))]
;;        [else
;;         #'(#%app proc-id . args)])]
;;     [(app . more)
;;      (syntax/loc stx
;;        (#%app . more))]))

;; ;;;
;; ;;; #%module-begin - Wrapper of the module-begin context
;; ;;; 

;; ; SYNTAX  (sketching-module-begin form ...)

;; ; Like #%module-begin, but additionally
;; ;   - initializes gui
;; ;   - defines default values for draw and setup
;; ;   - default values for mouse and key event handlers
;; ;   - starts event loop

;; (define-syntax (sketching-module-begin stx)
;;   (syntax-parse stx
;;     [(_sketching-module-begin def/expr ...)
;;      (define ctx (car (syntax->list #'(def/expr ...))))
;;      (with-syntax ([initialize        (datum->syntax ctx 'initialize)]
;;                    [setup             (datum->syntax ctx 'setup)]
;;                    [draw              (datum->syntax ctx 'draw)]
;;                    [on-mouse-pressed  (datum->syntax ctx 'on-mouse-pressed)]
;;                    [on-mouse-released (datum->syntax ctx 'on-mouse-released)]
;;                    [on-mouse-moved    (datum->syntax ctx 'on-mouse-moved)]
;;                    [on-mouse-dragged  (datum->syntax ctx 'on-mouse-dragged)]
;;                    [on-key-pressed    (datum->syntax ctx 'on-key-pressed)]
;;                    [on-key-released   (datum->syntax ctx 'on-key-released)]
;;                    [default-setup     (datum->syntax ctx 'default-setup)]
;;                    [default-draw      (datum->syntax ctx 'default-draw)])
;;      (syntax/loc stx
;;        (#%module-begin
;;         (initialize) ; setup frame, canvas and drawing context (pen, brush)
;;         def/expr ...
;;         (define (default-setup) (void))
;;         (define (default-draw)  (void))        
;;         (setup)
;;         (current-draw draw)
;;         (current-on-mouse-pressed  on-mouse-pressed)
;;         (current-on-mouse-released on-mouse-released)
;;         (current-on-mouse-moved    on-mouse-moved)
;;         (current-on-mouse-dragged  on-mouse-dragged)
;;         (current-on-key-pressed    on-key-pressed)
;;         (current-on-key-released   on-key-released)
;;         (start) ; start event loop
;;         )))]))

;; ;;;
;; ;;; struct - Structure declations
;; ;;; 

;; ; SYNTAX  (sketching-struct id maybe-super (field ...) struct-option ...)

;; ; Like struct but 
;; ;   - all structs are transparent
;; ;   - all fields are mutable


;; (define-syntax (sketching-struct stx)
;;   (syntax-parse stx
;;     [(_sketching-struct id:id super:id (field:id ...) struct-option ...)
;;      (with-syntax ([id? (format-id #'id "~a?" #'id)])
;;        (syntax/loc stx
;;          (begin
;;            (struct id super (field ...) #:transparent #:mutable struct-option ...)
;;            ; todo: fields of the super structs aren't declared here
;;            (declare-struct-fields id id? (field ...)))))]
;;     [(_sketching-struct id:id (field:id ...) struct-option ...)
;;      (with-syntax ([id? (format-id #'id "~a?" #'id)])
;;        (syntax/loc stx
;;          (begin
;;            (struct id (field ...) #:transparent #:mutable struct-option ...)
;;            (declare-struct-fields id id? (field ...)))))]
;;     [(_sketching-struct . more)
;;      (syntax/loc stx
;;        ; let struct handle error messages
;;        (struct . more))]))

;; ;;;
;; ;;; Exports
;; ;;;


;; (provide
;;  ;;; The base Racket language
;;  (except-out (all-from-out racket/base)
;;              random          ; the random in P differs from the standard Racket one
;;              #%module-begin  
;;              #%top
;;              #%app
;;              struct)
;;  (all-from-out racket/format)
;;  ;;; Our versions the following:
;;  (rename-out [sketching-module-begin #%module-begin])
;;  (rename-out [sketching-top          #%top])
;;  (rename-out [sketching-struct       struct])
;;  (rename-out [sketching-app          #%app])
;;  (rename-out [Class                  class])

 
;;  Object
;;  (all-from-out racket/class) ; class wasn't imported here
;;  ; define-method
;;  declare-struct-fields
;;  define-method
;;  ; dot-assign-field ; temp - just for debug

;;  ;;; Sketching specific bindings
 
;;  alpha
;;  arc
;;  background
;;  begin-shape
;;  bezier
;;  blue
;;  brightness
;;  circle
;;  color
;;  color-mode
;;  color?
;;  create-image
;;  ellipse
;;  ellipse-mode
;;  end-shape
;;  fill 
;;  green
;;  hue 
;;  image
;;  image-mode
;;  image-set
;;  image-get
;;  image-width
;;  image-height
;;  key
;;  key-pressed  
;;  key-released 
;;  lerp-color
;;  line
;;  load-image
;;  mouse-x
;;  mouse-y
;;  mouse-button  
;;  mouse-pressed
;;  no-fill
;;  no-gui
;;  no-smooth
;;  no-stroke
;;  no-tint
;;  pmouse-x
;;  pmouse-y
;;  point
;;  quad
;;  rect
;;  rect-mode
;;  red
;;  saturation 
;;  smoothing   ; Racket only
;;  square
;;  stroke
;;  stroke-weight
;;  stroke-join
;;  stroke-cap
;;  text
;;  text-align
;;  text-size
;;  text-face
;;  text-family
;;  text-weight
;;  text-underlined?
;;  text-smoothing
;;  text-size-in-pixels?
;;  text-hinting
;;  tint
;;  triangle
;;  vertex
 
;;  ;; Math
;;  abs
;;  atan2
;;  ceil
;;  constrain
;;  degrees
;;  dist
;;  floor
;;  lerp
;;  mag
;;  remap ; called map in P
;;  norm
;;  pow
;;  radians
;;  (rename-out [new-random random])
;;  round
;;  ; round-up
;;  sq
;;  sqr
;;  ;; Math Operators
;;  += -= *= /=
;;  := ++ --
;;  ;; Time and Date
;;  year month day hour minute second
;;  millis
;;  ;; Math Constants
;;  pi   π
;;  pi/2 π/2
;;  pi/4 π/4
;;  2pi 2π

;;  ;; Transform
;;  rotate
;;  scale
;;  translate
;;  push-matrix
;;  pop-matrix
;;  reset-matrix

;;  ;; Conversion
;;  binary
;;  char
;;  int
;;  unbinary

;;  ;; Environment / Gui
;;  cursor
;;  nap ; sleep in milliseconds
;;  display-density
;;  focused
;;  frame-count
;;  frame-rate
;;  set-frame-rate!
;;  delta-time
;;  fullscreen
;;  height
;;  loop
;;  no-cursor
;;  no-loop
;;  ; todo: no-smooth
;;  ; todo: smooth
;;  pixel-density
;;  pixel-height
;;  pixel-width
;;  load-pixels
;;  update-pixels
;;  set-pixel

;;  set-title
;;  size
;;  width

;;  initialize
;;  start
;;  )

;; ;;;
;; ;;; System Variables
;; ;;;

;; ; The follow "variables" are in Processing called system variables.
;; ; In Sketching they are represented as parameters, but we add a little
;; ; syntax, so we can write  width  instead of (width).


;; (require (for-syntax racket/base))
;; (define-syntax (width _)         #'(current-width))
;; (define-syntax (height _)        #'(current-height))
;; (define-syntax (focused _)       #'(focused?))
;; (define-syntax (frame-count _)   #'(current-frame-count))
;; (define-syntax (mouse-x _)       #'(current-mouse-x))
;; (define-syntax (mouse-y _)       #'(current-mouse-y))
;; (define-syntax (pmouse-x _)      #'(current-pmouse-x))
;; (define-syntax (pmouse-y _)      #'(current-pmouse-y))
;; (define-syntax (mouse-button _)  #'(current-mouse-button))
;; (define-syntax (mouse-pressed _) #'(current-mouse-pressed))
;; (define-syntax (key _)           #'(current-key))
;; (define-syntax (key-pressed _)   #'(current-key-pressed))
;; (define-syntax (key-released _)  #'(current-key-released))

;; (define-syntax (frame-rate stx)
;;   (syntax-case stx ()
;;     [(_frame-rate fps)            #'(set-frame-rate fps)]
;;     [id (identifier? #'id)        #'(actual-frame-rate)]
;;     [_ (error 'frame-rate "")]))

;; (define-syntax (set-frame-rate! stx)
;;   (syntax-case stx ()
;;     [(_set-frame-rate fps)  #'(set-frame-rate fps)]
;;     [_ (error 'set-frame-rate "")]))

;; ;;;
;; ;;; Initialization
;; ;;;

;; (define (initialize)
;;   ; (displayln 'initialize)
;;   (current-density (display-density))
;;   (initialize-gui)
;;   (void))

;; ;;;
;; ;;; Start Event Loop
;; ;;;

;; (define (start)
;;   ; (displayln 'start)
;;   (start-gui)
;;   (void))
