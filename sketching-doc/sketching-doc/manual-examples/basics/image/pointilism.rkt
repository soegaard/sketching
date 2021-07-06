#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Image/Pointillism/Pointillism.pde
; Pointillism.
; Original Processing example by Daniel Shiffman. 

; Mouse horizontal location controls size of dots. 
;  Creates a simple pointillist effect using ellipses colored
;  according to pixels in an image. 

(define img (load-image "moonwalk.jpg"))

(define small-point 4)
(define large-point 40)

(define (setup)
  (size 640 360)
  (frame-rate 120)
  (image-mode 'center)
  (no-stroke)
  (background 255))

(define (draw)
  (define radius (remap mouse-x 0 width small-point large-point))
  (define x      (int (random (image-width img))))
  (define y      (int (random (image-height img))))
  (define pix    (image-get img x y))
  (fill pix 128)
  (circle x y radius))
