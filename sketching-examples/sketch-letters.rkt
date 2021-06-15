#lang sketching
(require racket/string)


(define (setup)
  (size 710 400)
  (text-face "Apple Chancery")
  (text-size 32))

(define (draw)
  (background 160)
  
  (define gap    52)
  (define margin 10)

  ; (text-align 'left 'top)
  ; (text (number->string delta-time) 0 0)
  ; (text (number->string (int (/ 1000. delta-time))) 0 0)
  
  (translate (* 4 margin) (* 4 margin))

  (text-align 'center 'center)
  (define counter (int 35))
  (for* ([y (in-range 0 (- height gap) gap)]
         [x (in-range 0 (- width  gap) gap)])
    ; (line 0 y width y) (line x 0 x height)
    (define letter (char counter))
    (if (string-contains? "aeiouAEIOU" (string letter))
        (fill "#ed225d")  ; wovels
        (fill 255))      ; other
    (text letter x y)
    (++ counter)))


    
  
