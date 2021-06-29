#lang sketching

(define (setup)
  (size 600 600)
  (set-frame-rate! 30)
  (rect-mode 'center)
  (no-loop)
  )

(define (draw)
  (background 255)
  (stroke 0)

  
  
  (for ([x (in-range 150 width 200)])
    (line x 0 x height))
  (for ([y (in-range 150 width 200)])
    (line 0 y width y))

  (text-size 32)
  
  (text-align 'center 'center) (fill "red")
  (rect-mode 'corner) (text "cc-cor" 150 150)
  ;(rect-mode 'center) (text "cc-cen" 350 150)

  (text-align 'left 'baseline) (fill "blue")  
  (rect-mode 'corner) (text "cbase-cor" 150 350)
  ;(rect-mode 'center) (text "cbase-cen" 350 350)
  
  (text-align 'right 'bottom)  (fill "green")
  (rect-mode 'corner) (text "rbot-corn" 150 550)
  ;(rect-mode 'center) (text "rbot-cen" 350 550)
  )

  


  
