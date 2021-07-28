#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/WaveGradient/WaveGradient.pde

;; Wave Gradient
;
;; orignal Processing example by Ira Greenberg.  
;; Generate a gradient along a sin() wave.


(define amplitude 30)
(define fill-gap 2.5)

(define (setup)
  (size 640 360)
  ; (background 200)
  (no-loop))

(define (draw)
  (define w/2 width)
  (define h/2 height)
  ;; To efficiently set all the pixels on screen, make the set() 
  ;; calls on a PImage, then write the result to the screen.
  (define gradient (create-image width height 'rgb))
  (define frequency 0)
  
  (for ([i (in-range -75 (+ height 75))])
    ;; Reset angle to 0, so waves stack properly
    (define angle 0)
    ;; Increasing frequency causes more gaps
    (+= frequency 0.002)
    (for ([j (+ width 75)])
      (define py (+ i (* (sin (radians angle)) amplitude)))
      (+= angle frequency)
      (define c (color (abs   (/ (* (- py i) 255) amplitude))
                       (- 255 (/ (* (abs (- py i)) 255) amplitude))
                       (min 255 (* j (/ 255 (+ width 50))))))
      ;; Hack to fill gaps. Raise value of fillGap if you increase frequency
      (for ([filler (in-range fill-gap)])
        (image-set gradient (+ w/2 (int (- j filler))) (+ h/2 (- (int py) filler)) c)
        (image-set gradient (+ w/2 (int    j))         (+ h/2    (int py))         c)
        (image-set gradient (+ w/2 (int (+ j filler))) (+ h/2 (+ (int py) filler)) c))))
  ;; Draw the image to the screen
  (image gradient 0 0))
