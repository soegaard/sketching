#lang sketching

(define cursors '(arrow bullseye cross hand ibeam watch blank
                        size-n/s size-e/w size-ne/sw size-nw/se))

(define (setup)
  (size 500 100)
  (cursor 'arrow))

(define (draw)
  ; draw vertical strips
  (define n (length cursors))
  (define w (/ width n))
  (stroke 255)
  (for ([x (in-range 0 width w)])
    (fill (floor (lerp 0 255 (/ x width))))
    (rect x 0 w height))
  ; change cursor
  (define index (constrain (floor (/ mouse-x w)) 0 (- n 1)))
  (define c     (list-ref cursors index))
  ; write chosen cursor
  (text-size 30)
  (fill 255)
  (text (~a c) 200 25)
  (cursor c))
