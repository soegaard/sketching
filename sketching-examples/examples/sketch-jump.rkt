#lang sketching
(require racket/format)

; References:
;   https://2dengine.com/?p=platformers

; If a is constant then:
;   y = - 1/2 a t² + v₀ t + y₀
; Here y₀=0 on the ground:
;   y = - 1/2 a t² + v₀ t
;   y= 0  <=>  0 = - 1/2 a t² + v₀ t  <=>  t=2v₀/a or t=0
; Time between jump start and end:  time = 2v₀/a
; For fixed jump time and initial velovity v₀ the acceleration is:
;   a = 2v₀/time
; Also:
;   v = - a t + v₀
; The velocity is zero at the top:
;  v=0  <=>  t = v₀/a
; The corresponding height is:
;              v₀²
;   height = -------
;              a
; If we fix both time between jump start and end as well as height, we get:
;        8 height               4 height 
;    a= ----------  and  v₀ = ------------
;          time²                time


; 1 px per one 60 fps frame:
; velocity = 1 / (1/60) = 60

;;;
;;; Model
;;;

(define man      #f)
(define ground-y #f)

(define walk-velocity 60)

(define jump-height     20) ; pixels
(define jump-time-up    0.50) ; seconds
(define jump-time-down  0.25) ; seconds

(define jump-velocity (- (/ (* 2. jump-height) jump-time-up)))
(define gravity-up    (/ (* 8 jump-height) (sqr (* 2. jump-time-up))))   ; moving up
(define gravity-down  (/ (* 8 jump-height) (sqr (* 2. jump-time-down)))) ; moving down

(define damping-x     2.)

;;; Body

(class Body Object
  (field [x  0.] [y 0.]             ; position
         [vx 0.] [vy 0.]            ; velocity
         [ax 0.] [ay gravity-down]  ; acceleration
         [m  1.])                   ; mass
  (super-new)

  (define/public (on-ground?)
    (>= y ground-y))

  (define/public (apply-vertical-force F)
    (+= ay (/ F m)))
  (define/public (apply-horizontal-force F)
    (+= ax (/ F m)))
  (define/public (apply-force Fx Fy)
    (+= ax (/ Fx m))
    (+= ay (/ Fy m)))

  (define ε 0.01)
  
  (define/public (update dt)
    ; dt is the time step
    (cond
      [(on-ground?) (:= ay 0.)]
      [(>= vy 0)    (:= ay gravity-down)]
      [else         (:= ay gravity-up)])
    ; Semi-implicit Euler integration
    (+= vx (* ax dt))  ; update velocity before position
    (+= vy (* ay dt))
    (+=  x (* vx dt))
    (+=  y (* vy dt))
    ; Boundaries
    (when (on-ground?)
      (:= y ground-y)
      (:= vy 0.)
      ; Friction (damping) in horisontal direction
      (when (eq? key 'release)
        (/= vx (+ 1. (* damping-x dt)))))
    (when (< (abs vx) ε)
      (:= vx 0.))))
      

;;; Man

(class Man Body
  (init m x y)
  (init-field size)
  
  (super-new)
  (:= this.x x)
  (:= this.y x)
  (:= this.m m)

  (define/public (jump)
    (+= this.vy jump-velocity))
  (define/public (right)
    (:= this.vx walk-velocity))
  (define/public (left)
    (:= this.vx (- walk-velocity)))

  (define/override (update dt)
    (super update dt)
    (when (< this.x 0)     (+= this.x width))
    (when (> this.x width) (-= this.x width)))
  
  (define/public (draw)
    (fill 0)
    (rect-mode 'center)
    (rect this.x this.y size size)))

;;; Setup and update

(define (setup-model)
  (:= ground-y (* 4/5 height))
  (:= man (new Man [m 1.] [x (/ width  2.)] [y (/ height 4.)] [size 8]))
  (:= man.vx 0))

(define left-over-time 0.)
(define (update-model dt)
  ; (display ".")
  ; dt is passed time in seconds
  ; st = step time = time pr model step
  ; (displayln (list 'dt dt))
  (define st (/ 1. 100))
  (let loop ([dt (+ left-over-time dt)])
    (cond
      [(< dt st) (:= left-over-time dt)]
      [else      (man.update st)
                 (loop (- dt st))])))

;;;
;;; CONTROLLER
;;;

(define (mouse-pressed)
  (displayln "MOUSE")
  (man.jump))

(define (key-pressed)
  ; (display "KEY: ") (write key) (newline)
  (case key
    [(#\space) (man.jump)]
    [(right)   (man.right)]
    [(left)    (man.left)]))

;;;
;;; VIEW
;;;

(define (setup)
  (size 640 360)
  (frame-rate 30)
  (setup-model))
  
(define (draw)
  ;; Model
  (update-model (/ delta-time 1000.))
  ;; View
  (background 255)
  (line 0 (- ground-y jump-height) width (- ground-y jump-height))
  (fill "green")
  (no-stroke)
  (quad 0 ground-y  width ground-y  width height  0 height)
  (stroke 0)
  (line 0 ground-y width ground-y)

  (man.draw)

  (text (~a "x: "  man.x)   20  20)
  (text (~a "y: "  man.y)  120  20)
  (text (~a "vx: " man.vx)  20  80)
  (text (~a "vy: " man.vy) 120  80)
  (text (~a "ax: " man.ax)  20 140)
  (text (~a "ay: " man.ay) 120 140)  
  )
