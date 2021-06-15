#lang sketching

(define (setup)
  (size 640 360))

(define (draw)
  (fill 0 102 153)     ; text color
  (text-size 32)
  (text "Word1" 0 0)
  
  (fill 0 102 153)
  (text-size 64)
  (text "word2" 10 72)

  (text-size 128)
  (fill 0 102 153 10)
  (text "word3" 10 200))

