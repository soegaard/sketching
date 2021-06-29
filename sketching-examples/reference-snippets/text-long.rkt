#lang sketching

(define homer-1.1 
"Tell me, O Muse, of the man of many devices, who wandered full many
ways after he had sacked the sacred citadel of Troy. Many were the men
whose cities he saw and whose mind he learned, aye, and many the woes
he suffered in his heart upon the sea, [5] seeking to win his own life
and the return of his comrades. Yet even so he saved not his comrades,
though he desired it sore, for through their own blind folly they
perished—fools, who devoured the kine of Helios Hyperion; but he took
from them the day of their returning. [10] Of these things, goddess,
daughter of Zeus, beginning where thou wilt, tell thou even unto us.")

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
  
  (text-align 'left 'baseline) (fill "red")
  (rect-mode 'corner)
  (text-face "Monaco")

  (text "foo" 150 150)

  (text-align 'center 'top) (fill "red")
  (rect-mode 'center)
  (text homer-1.1 150 350 200 200)
  (stroke "blue")
  (no-fill)
  (rect 150 350 200 200)


  
  )
