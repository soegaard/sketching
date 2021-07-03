#lang sketching
42

(define v (vector 00 11 22 33 44 55))
(v.length)

;; (struct blob (x y))

;; (define-method blob blob?
;;   (draw this [color "red"])
;;   (circle (blob-x this) (blob-y this)))


;; (define b (blob 100 100))

;; (define (setup)
;;   (size 640 360))

;; (define (draw)
;;   42
;;   (b.draw))


(define xss (vector (vector 1 2)
                    (vector 'a 'b)))

(for*/list ([i 2] [j 2])
  (xss.ref i j))
