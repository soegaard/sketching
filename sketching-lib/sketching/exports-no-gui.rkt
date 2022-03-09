#lang racket/base
;;;
;;; Exports no GUI
;;;

; This file exports all bindings of the Sketching language
; except those depending on racket/gui (i.e. "gui.rkt" and "environment.rkt").

;;;
;;; Sketching
;;;

;;;
;;; Sketching - as a #lang language
;;;

; The user writes a standard module with expressions and definition in some order. 
;   #lang sketching
;   ...
;   (define (draw) ...)
;   (define (setup) ...)

; Our module-begin turns this into

;   #lang sketching
;   ...
;   (define (draw) ...)
;   (define (setup) ...)
;   (setup) (current-draw draw) (start)

; Now the user can omit draw and setup and in that case we need to use
; a default. We use #%top to catch these.


;;;
;;; Imports
;;;

(require (for-syntax syntax/parse racket/base syntax/strip-context racket/string
                     racket/syntax
                     "syntax-utils.rkt")
         (except-in racket/class class)
         (only-in racket/vector vector-copy)
         racket/format
         "class.rkt"          
         "color.rkt"
         "conversion.rkt"
         "dot-method.rkt"
         "environment-no-gui.rkt"
         ; "environment.rkt" ; depends on racket/gui
         ; "gui.rkt"         ; depends on racket/gui
         "graphics.rkt"
         "math.rkt"
         "math-operators.rkt"
         "noise.rkt"
         "parameters.rkt"
         "time-and-date.rkt"
         "transform.rkt"
         "shape.rkt")




;;;
;;; #%top - Top-level variable references
;;; 

; SYNTAX  (sketching-top . id)

;   Like #%top, but additionally:
;     - identifiers containing a dot is rewritten to use dot-field,
;       enabling  (#%app foo.bar 1 2) -> (send foo bar 1 2 )
;       and       foo.bar             -> (get-field foo bar)
;     - default values for the event handlers:
;          on-mouse-pressed, on-mouse-released, on-mouse-moved, on-mouse-dragged,
;          on-key-pressed, on-key-released
;     - default values for  setup  and  draw

; Note: sketching-top is exported as #%top.

(define-syntax (sketching-top stx)
  (syntax-parse stx
    [(top . top-id)
     (with-syntax ([default-setup (datum->syntax #'top 'default-setup)]
                   [default-draw  (datum->syntax #'top 'default-draw)])
       (case (syntax->datum #'top-id)
         [(setup)             #'default-setup]
         [(draw)              #'default-draw]
         [(on-mouse-pressed)  #'#f]
         [(on-mouse-released) #'#f]
         [(on-mouse-moved)    #'#f]
         [(on-mouse-dragged)  #'#f]
         [(on-key-pressed)    #'#f]
         [(on-key-released)   #'#f]
         [else
          (cond
            [(or (id-contains? #'top-id ".") (id-contains? #'top-id "_"))
             (with-syntax ([(id ...) (map number-id->number 
                                          (split-id-at-dot/underscore #'top-id))])
               (syntax/loc stx (dot/underscore id ...)))]
            [else
             #'(#%top . top-id)])]))]))

;;;
;;; #%app - Procedure application
;;; 

; SYNTAX  (sketching-app proc-expr arg ...)

;   Like #%app, but additionally:
;     - a call to  o.m  where o is an object and m is a mtheod name,
;       is rewritten to a method call
;     - a call to  f, where the identifier f contains no dots,
;       is a normal call.

(define-syntax (sketching-app stx)
  (syntax-parse stx
    ; calls to a   o.m is a method call
    ; calls to an  f   is a normal call
    [(app proc-id:id . args)
     ; (display ".")
     (define str (symbol->string (syntax-e #'proc-id)))
     (cond
       [(string-contains? str ".")
        (define ids (map string->symbol (string-split str ".")))
        (with-syntax ([(id ...) (for/list ([id ids])
                                  (datum->syntax #'proc-id id))])
          #'(app-dot-method (id ...) . args))]
       [else
        #'(#%app proc-id . args)])]
    [(app . more)
     (syntax/loc stx
       (#%app . more))]))




;;;
;;; struct - Structure declations
;;; 

; SYNTAX  (sketching-struct id maybe-super (field ...) struct-option ...)

; Like struct but 
;   - all structs are transparent
;   - all fields are mutable


(define-syntax (sketching-struct stx)
  (syntax-parse stx
    [(_sketching-struct id:id super:id (field:id ...) struct-option ...)
     (with-syntax ([id? (format-id #'id "~a?" #'id)])
       (syntax/loc stx
         (begin
           (struct id super (field ...) #:transparent #:mutable struct-option ...)
           ; todo: fields of the super structs aren't declared here
           (declare-struct-fields id id? (field ...)))))]
    [(_sketching-struct id:id (field:id ...) struct-option ...)
     (with-syntax ([id? (format-id #'id "~a?" #'id)])
       (syntax/loc stx
         (begin
           (struct id (field ...) #:transparent #:mutable struct-option ...)
           (declare-struct-fields id id? (field ...)))))]
    [(_sketching-struct . more)
     (syntax/loc stx
       ; let struct handle error messages
       (struct . more))]))

;;;
;;; #%ref - Indexing
;;; 

; SYNTAX  (sketching-ref id expr)
; SYNTAX  (sketching-ref id expr expr)

(define-syntax (sketching-ref stx)
  (syntax-parse stx
    [(_ref id:id e:expr)
     (syntax/loc stx
       (let ([v id] [i e])
         (cond
           [(vector? v) (vector-ref v i)]
           [(string? v) (string-ref v i)]
           [(hash? v)   (hash-ref v i)]
           [(list? v)   (list-ref v i)]
           [else        (raise-argument-error
                         '#%ref "expected a vector, string, list or hash, got: "
                         v)])))]
    [(_ref id:id e1:expr e2:expr)
     (syntax/loc stx
       (let ([v id] [i e1] [j e2])
         (cond
           [(vector? v) (if j
                            (vector-copy v i j)
                            (vector-copy v i (string-length v)))]
           [(string? v) (cond
                          [(and i j) (substring v i j)]
                          [j         (substring v 0 j)]
                          [i         (substring v i (string-length v))]
                          [else      (substring v 0 (string-length v))])]
           [(hash? v)   (hash-ref v i j)]
           [else        (raise-argument-error
                         '#%ref "two argument indexing works for hash tables, got: "
                         v)])))]))

;;;
;;; Exports
;;;


(provide
 ;;; The base Racket language
 (except-out (all-from-out racket/base)
             random          ; the random in P differs from the standard Racket one
             #%module-begin  
             #%top
             #%app             
             struct)
 (all-from-out racket/format)
 ;;; Our versions the following:
 ; gui (rename-out [sketching-module-begin #%module-begin])
 (rename-out [sketching-top          #%top])
 (rename-out [sketching-struct       struct])
 (rename-out [sketching-app          #%app])
 (rename-out [sketching-ref          #%ref])
 (rename-out [Class                  class])

 
 Object
 (all-from-out racket/class) ; class wasn't imported here
 ; (except-out (all-from-out "class.rkt") class)
 ; define-method
 declare-struct-fields
 define-method
 ; dot-assign-field ; temp - just for debug
 
 ; gui no-gui
 
 
 ;;; Sketching specific bindings
 
 alpha
 arc
 background
 begin-shape
 bezier
 blue
 brightness
 circle
 color
 color-mode
 color?
 create-image
 ellipse
 ellipse-mode
 end-shape
 fill 
 green
 hue 
 image
 image-mask
 image-mode
 image-set
 image-get
 image-width
 image-height
 key
 key-pressed  
 key-released 
 lerp-color
 line
 load-image
 mouse-x
 mouse-y
 mouse-button  
 mouse-pressed
 no-fill
 no-smooth
 no-stroke
 no-tint
 pmouse-x
 pmouse-y
 point
 quad
 rect
 rect-mode
 red
 saturation 
 smoothing   ; Racket only
 square
 stroke
 stroke-weight
 stroke-join
 stroke-cap
 text
 text-align
 text-size
 text-face
 text-family
 text-weight
 text-underlined?
 text-smoothing
 text-size-in-pixels?
 text-hinting
 tint
 triangle
 vertex

 ;; Shape
 Shape
 create-shape
 
 ;; Math
 abs
 atan2
 ceil
 constrain
 degrees
 dist
 floor
 lerp
 mag
 remap ; called map in P
 norm
 pow
 radians
 (rename-out [new-random random])
 round
 ; round-up
 sq
 sqr
 ;; Math Operators
 += -= *= /=
 := ++ --
 ;; Time and Date
 year month day hour minute second
 millis
 ;; Math Constants
 pi   π
 pi/2 π/2
 pi/4 π/4
 2pi 2π

 ;; Transform
 rotate
 scale
 translate
 push-matrix
 pop-matrix
 reset-matrix
 get-matrix
 set-matrix
 shear-x
 shear-y
 apply-matrix
 
 ;; Conversion
 binary
 char
 hex
 int
 unbinary 
 unhex

 ;; Noise
 noise
 simplex-noise
 
 ;; Environment / Gui
 ; gui cursor
 ; gui nap ; sleep in milliseconds
 ; gui display-density
 focused
 frame-count
 ; gui frame-rate
 ; gui set-frame-rate!
 delta-time
 ; gui fullscreen
 height
 ; gui loop
 ; gui no-cursor
 ; gui no-loop
 ; todo: no-smooth
 ; todo: smooth
 ; gui pixel-density
 pixel-height
 pixel-width
 ; gui load-pixels
 ; gui update-pixels
 ; gui set-pixel

 ; gui set-title
 size
 width

 ; gui initialize
 ; gui start

 with-dc
 )

;;;
;;; System Variables
;;;

; The follow "variables" are in Processing called system variables.
; In Sketching they are represented as parameters, but we add a little
; syntax, so we can write  width  instead of (width).


(require (for-syntax racket/base))
(define-syntax (width _)         #'(current-width))
(define-syntax (height _)        #'(current-height))
(define-syntax (focused _)       #'(focused?))
(define-syntax (frame-count _)   #'(current-frame-count))
(define-syntax (mouse-x _)       #'(current-mouse-x))
(define-syntax (mouse-y _)       #'(current-mouse-y))
(define-syntax (pmouse-x _)      #'(current-pmouse-x))
(define-syntax (pmouse-y _)      #'(current-pmouse-y))
(define-syntax (mouse-button _)  #'(current-mouse-button))
(define-syntax (mouse-pressed _) #'(current-mouse-pressed))
(define-syntax (key _)           #'(current-key))
(define-syntax (key-pressed _)   #'(current-key-pressed))
(define-syntax (key-released _)  #'(current-key-released))



