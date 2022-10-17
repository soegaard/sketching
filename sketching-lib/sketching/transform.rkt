#lang racket/base
(require racket/draw
         racket/class
         racket/vector
         cairo
         "parameters.rkt")

(provide rotate
         scale
         translate
         push-matrix
         pop-matrix
         reset-matrix
         get-matrix
         get-current-matrix
         set-matrix
         apply-matrix
         shear-x
         shear-y
         translation-matrix
         scaling-matrix
         rotation-matrix
         new-matrix
         multiply-matrix
         matrix->transformation
         apply-matrix-to-point
         transformation->matrix
         transformation->vector
         transformation)

;;; Implementation Notes

; The drawing context dc contains the current transformation,
; which consist has the form:
;     (vector (vector a b c d e f) x-origin y-origin x-scale y-scale rotation)
; We will *exclusively* use the affine transformation represented by the
; vector  (vector a b c d e f)  to implement the various transformations.

; We will use Matrix from cairo to represent the affine transformation.

(define (matrix->transformation matrix)
  (define vec (send matrix ->vector))
  (vector vec 0. 0. 1. 1. 0.))

(define (transformation->matrix t)
  (define v (vector-ref t 0))
  (define M (new Matrix))
  (send M init
        (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)
        (vector-ref v 3) (vector-ref v 4) (vector-ref v 5))
  M)

(define (get-current-matrix)
  (transformation->matrix (send dc get-transformation))) 

(define (apply-matrix-to-point M x y)
  (define v (send M ->vector))
  (define-values (xx yx xy yy x0 y0)
    (values (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)
            (vector-ref v 3) (vector-ref v 4) (vector-ref v 5)))
  (values (+ (* xx x) (* xy y) x0)
          (+ (* yx x) (* yy y) y0)))


; Cairo has a few builtin transformations

(define (rotation-matrix angle)
  (define M (new Matrix))
  (send M init-rotate angle)
  M)

(define (translation-matrix dx dy)
  (define M (new Matrix))
  (send M init-translate dx dy)
  M)

(define (scaling-matrix sx sy)
  (define M (new Matrix))
  (send M init-scale sx sy)
  M)

(define (shearing-matrix sx sy)
  (define M (new Matrix))
  (send M init 1. sy sx 1. 0. 0.)
  M)

(define (new-matrix a b c d e f)
  (define M (new Matrix))
  (send M init a b c d e f)
  M)
  

;;;
;;; Sketching Transformations
;;;

(define (rotate angle) ; radians
  ; Note: We can't use (send mat rotate angle), since
  ;       that will apply the rotation before rather than after.
  (define mat (get-current-matrix))
  (define rot (rotation-matrix angle))
  (send mat multiply rot mat)
  (send dc set-transformation (matrix->transformation mat)))

(define (translate dx dy)
  ; This is is a debug version, to check whether the version
  ; below is correct.
  (define mat (get-current-matrix))
  (define tra (translation-matrix dx dy))
  (send mat multiply tra mat)
  (send dc set-transformation (matrix->transformation mat)))

#;(define (translate dx dy)
  ; We could implement `translate` in the same way as `rotate`,
  ; we do it directly in the name of efficiency.
  
  ; We need copies here, since we get immutable vectors back.
  (define t (vector-copy (send dc get-transformation)))
  (define v (vector-copy (vector-ref t 0)))
  (vector-set! v 4 (+ (vector-ref v 4) dx))
  (vector-set! v 5 (+ (vector-ref v 5) dy))
  (vector-set! t 0 v)
  (send dc set-transformation t))

(define scale
  (case-lambda
    [(s)     (scale s s)]
    [(sx sy) (define mat (get-current-matrix))
             (define sca (scaling-matrix sx sy))
             (send mat multiply sca mat)
             (send dc set-transformation (matrix->transformation mat))]
    [else (error 'scale "incorrect arguments")]))


(define (shear-x sx) 
  (define mat (get-current-matrix))
  (define shx (shearing-matrix sx 0.))
  (send mat multiply shx mat)
  (send dc set-transformation (matrix->transformation mat)))

(define (shear-y sy) 
  (define mat (get-current-matrix))
  (define shy (shearing-matrix 0. sy))
  (send mat multiply shy mat)
  (send dc set-transformation (matrix->transformation mat)))

(define (apply-matrix a b c d e f)
  (define mat (get-current-matrix))
  (define app (new-matrix a b c d e f))
  (send mat multiply app mat)
  (send dc set-transformation (matrix->transformation mat)))

(define (multiply-matrix2 m1 m2)
  (define M (new Matrix))
  (send M multiply m2 m1) ; sigh: cairy uses a strange order
  M)

(define (multiply-matrix m . ms)
  (let loop ([ms (cons m ms)])
    (cond
      [(null? (cdr ms)) (car ms)]
      [else             (multiply-matrix2 (car ms) (loop (cdr ms)))])))

;;; Stack of transformations

(define the-matrix-stack '())

(define (push! m)
  (set! the-matrix-stack (cons m the-matrix-stack)))

(define (pop!)
  (define m (car the-matrix-stack))
  (set! the-matrix-stack (cdr the-matrix-stack))
  m)

(define (push-matrix)
  (push! (send dc get-transformation)))

(define (pop-matrix)
  (define m (pop!))
  (send dc set-transformation m))


(define (reset-matrix)
  ; (define initial (send dc get-initial-matrix))
  (define initial (vector 1. 0. 0. 1. 0. 0.))
  (send dc set-transformation (vector initial 0. 0. 1. 1. 0.)))

(struct transformation (vec) #:transparent) 

(define (transformation->vector t)
  (transformation-vec t))

(define (get-matrix)
  (transformation (send dc get-transformation)))

(define (set-matrix t)
  (unless (transformation? t)
    (error 'set-matrix "expected a transformation, got: ~a" t))
  (send dc set-transformation (transformation-vec t)))
