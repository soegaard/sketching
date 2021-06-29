#lang sketching
;; Press the mouse to hide the cursor

(define (draw)
  (if mouse-pressed
      (no-cursor)
      (cursor 'hand)))

