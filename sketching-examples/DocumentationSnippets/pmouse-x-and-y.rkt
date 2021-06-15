#lang sketching

(define (draw)
  (background 204)
  (line 20 mouse-y  80 pmouse-y )
  (displayln (~a mouse-y " : " pmouse-y)))

