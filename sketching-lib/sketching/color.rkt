#lang racket/base
(require racket/draw
         racket/class
         racket/match
         racket/format
         racket/math
         "math.rkt"
         "parameters.rkt")

(provide args->color
         color?
         color
         color-mode
         lerp-color
         red green blue alpha
         hue saturation brightness
         red255 green255 blue255 alpha1
         )

(define (color? x)
  (and (object? x)
       (is-a? x color%)))

(define max1 255)
(define max2 255)
(define max3 255)
(define maxA 255)

(define color-mode
  (case-lambda
    [(mode)
     (current-color-mode mode)]
    [(mode max)
     (current-color-mode mode)
     (set! max1 max)
     (set! max2 max)
     (set! max3 max)
     (set! maxA max)]
    [(mode m1 m2 m3)
     (current-color-mode mode)
     (set! max1 m1)
     (set! max2 m2)
     (set! max3 m3)]
    [(mode m1 m2 m3 mA)
     (current-color-mode mode)
     (set! max1 m1)
     (set! max2 m2)
     (set! max3 m3)
     (set! maxA mA)]))

(define (mod2 x)
  (define i (truncate x))
  (define f (- x i))
  (+ (modulo i 2) f))

(define (hsb->rgb h s b)
  ; 0 <= h 360, 0<=S<=1, 0<=b<=1
  (define c (* b s))
  (define x (* c (- 1 (abs (- (mod2 (/ h 60)) 1)))))
  (define m (- b c))
  (define-values (r1 g1 b1)
    (let ()
      (define (bet f x t) (and (<= f x) (< x t)))
      (cond
        [(bet  0  h  60) (values c x 0)]
        [(bet  60 h 120) (values x c 0)]
        [(bet 120 h 180) (values 0 c x)]
        [(bet 180 h 240) (values 0 x c)]
        [(bet 240 h 300) (values x 0 c)]
        [(bet 300 h 360) (values c 0 x)]
        [else            (error 'hsb->rgb)])))
  (values (inexact->exact (round (* 255. (+ r1 m))))
          (inexact->exact (round (* 255. (+ g1 m))))
          (inexact->exact (round (* 255. (+ b1 m))))))

(define (rgb->hsb r g b)
  ; Input:  r g b in 0..255
  ; Output: h in [0;360], s v in [0;1]
  ;         if s=0 then #f (for undefined)
  (set! r (/ r 255.))
  (set! g (/ g 255.))
  (set! b (/ b 255.))

  (define mx (max r g b))
  (define mn (min r g b))
  (define d  (- mx mn))

  (define v mx) ; v = brightness
  (define s (if (= mx 0) 0 (/ d mx)))
  (define h ; hue
    (cond
      [(= s 0)  0]                     ; undefined, for convenience we use 0 here
      [(= r mx)      (/ (- g b) d)]    ; between yellow and magenta
      [(= g mx) (+ 2 (/ (- b r) d))]   ; between cyan and yellow
      [(= b mx) (+ 4 (/ (- r g) d))])) ; between magenta and cyan
  (when h
    (set! h (* 60 h)) ; convert to degrees
    (when (< h 0) (set! h (+ h 360))))
  (values h s v))


(define (args->color args who)
  ; normalize an argument to either 0-255 or 0.-1.0
  (define (nA x) (/ (min (max 0. x) maxA) maxA))
  (define (make x y z [a #f])
    (case (current-color-mode)
      [(rgb)   (define (n  x max) (inexact->exact (round (* 255 (/ x max)))))
               (define (n1 x max1) (/ (min (max 0. x) max1) max1))
               (if a
                   (make-object color% (n x max1) (n y max2) (n z max3) (n1 a maxA))
                   (make-object color% (n x max1) (n y max2) (n z max3)))]
      [(hsb) (define (n  x max) (modulo (inexact->exact (round (* 360 (/ x max)))) 360))
             (define (n1 x max) (/ x max))
             (define-values (r g b) (hsb->rgb (n x max1) (n1 y max2) (n1 z max3)))
             (if a
                 (make-object color% r g b (n1 a maxA))
                 (make-object color% r g b))]
      [else (error 'args->color "internal error: unsupported color mode, got: ~a"
                   (current-color-mode))]))
  (define (make-gray x [a #f])
    (define (n1 x) (inexact->exact (round (* 255. (/ (min (max 0. x) max1) max1)))))
    ; (define x1 (exact-floor (min 255 (max 0 x))))
    (define x1 (n1 x))
    (if a
        (make-object color% x1 x1 x1 (nA a))
        (make-object color% x1 x1 x1)))
  (define (string-color s)
    (cond
      [(and (string? s) (= (string-length s) 7) (eqv? (string-ref s 0) #\#))
       ; #xxxxxx  hexa decimal !
       (define r (hex (string-ref s 1) (string-ref s 2)))
       (define g (hex (string-ref s 3) (string-ref s 4)))
       (define b (hex (string-ref s 5) (string-ref s 6)))
       (make r g b)]
      [(and (string? s) (= (string-length s) 9) (eqv? (string-ref s 0) #\#))
       ; #xxxxxx  hexa decimal !
       (define r (hex (string-ref s 1) (string-ref s 2)))
       (define g (hex (string-ref s 3) (string-ref s 4)))
       (define b (hex (string-ref s 5) (string-ref s 6)))
       (define a (hex (string-ref s 7) (string-ref s 8)))       
       (make r g b a)]
      [else
       (make-object color% s)]))

  (define c0  (char->integer #\0))
  (define c9  (char->integer #\9))
  (define ca  (char->integer #\a))
  (define cf  (char->integer #\f))
  (define cA  (char->integer #\A))
  (define cF  (char->integer #\F))
  
  (define (hex1 x)
    (define x0 (char->integer x))
    (cond
      [(<= c0 x0 c9)       (- x0 c0)]
      [(<= ca x0 cf) (+ 10 (- x0 ca))]
      [(<= cA x0 cF) (+ 10 (- x0 cA))]
      [else          0]))
  (define (hex x y)
    (+ (* 16 (hex1 x)) (hex1 y)))
  
  (match args
    [(list (? color? c))            c]                                   ; color
    [(list (? real? x))             (make-gray x)]                       ; gray
    [(list (? integer? x) a)        (make-gray x a)]                     ; gray + α
    [(list r g b)                   (make r g b)]                        ; rgb 
    [(list r g b a)                 (make r g b a)]                      ; rgb + alpha
    [(list (? string? s))           (string-color s)]                    ; name
    [(list (? string? s) a)         (define c (string-color s))          ; name + α
                                    (make-object color% (do-red c) (do-green c) (do-blue c) (/ a maxA))]    
    [_                              (error who (~a "got: " args))]))

(define (color . args)
  (args->color args 'color))

(define (red255   c) (send c red))
(define (green255 c) (send c green))
(define (blue255  c) (send c blue))
(define (alpha1   c) (send c alpha))

(define (scale-color-component v max)
  (inexact->exact (round (* max (/ v 255.)))))
(define (scale-color-component1 v max)
  (inexact->exact (round (* max v))))

(define (red        . args)
  (define c (args->color args 'red))
  (define v (send c red))
  (if (= max1 255) v (scale-color-component v max1)))
(define (green        . args)
  (define c (args->color args 'green))
  (define v (send c green))
  (if (= max2 255) v (scale-color-component v max2)))
(define (blue        . args)
  (define c (args->color args 'blue))
  (define v (send c blue))
  (if (= max3 255) v (scale-color-component v max3)))
(define (alpha        . args)
  (define c (args->color args 'alpha))
  (define v1 (send c alpha))
  (if (= maxA 1.0)
      v1
      (scale-color-component (inexact->exact (* 255 v1)) maxA)))

(define (hue . args)
  (define c (args->color args 'hue))
  (define-values (h s b) (rgb->hsb (do-red c) (do-green c) (do-blue c)))
  (if (= max1 255) h (scale-color-component h max1)))

(define (saturation . args)
  (define c (args->color args 'saturation))
  (define-values (h s b) (rgb->hsb (do-red c) (do-green c) (do-blue c)))
  (if (= max2 1.0) s (scale-color-component1 s max2)))

(define (brightness . args)
  (define c (args->color args 'brightness))
  (define-values (h s b) (rgb->hsb (do-red c) (do-green c) (do-blue c)))
  (if (= max3 1.0) b (scale-color-component1 b max3)))

(define (do-red   c) (send c red))
(define (do-green c) (send c green))
(define (do-blue  c) (send c blue))
  

(define (exact-round x)
  (inexact->exact (round x)))

(define (lerp-color c1 c2 amt)
  (set! amt (min amt 1.))
  (set! amt (max amt 0.))
  (set! c1 (args->color (list c1) 'lerp-color))
  (set! c2 (args->color (list c2) 'lerp-color))
  (make-object color% 
               (exact-round (lerp (do-red   c1) (do-red   c2) amt))
               (exact-round (lerp (do-green c1) (do-green c2) amt))
               (exact-round (lerp (do-blue  c1) (do-blue  c2) amt))))
