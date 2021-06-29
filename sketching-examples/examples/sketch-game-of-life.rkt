#lang sketching

(define N 50)

(define board (for/vector ([i N])
                (for/vector ([j N])                  
                  (if (> (random 1) 0.5)
                      1 0))))

(define (ref i j)   (vector-ref (vector-ref board i) j))

(define (live? i j) (equal? (ref i j) 1))
(define (dead? i j) (equal? (ref i j) 0))

(define (live-neighbours i j)
  (define i-1 (- i 1))
  (define i+1 (+ i 1))
  (define j-1 (- j 1))
  (define j+1 (+ j 1))
  
  (+ (ref i-1 j-1) (ref i-1 j) (ref i-1 j+1)
     (ref i   j-1)             (ref i   j+1)
     (ref i+1 j-1) (ref i+1 j) (ref i+1 j+1)))


(define (update)
  (for/vector ([i N])
    (for/vector ([j N])
      (if (or (= i 0) (= i (- N 1))
              (= j 0) (= j (- N 1)))
          0
          (cond
            [(live? i j) (case (live-neighbours i j)
                           [(0 1) 0]   ; starvation
                           [(2 3) 1]   ; survives
                           [else  0])] ; over population
            [else        (case (live-neighbours i j)
                           [(3)   1]
                           [else  0])])))))

(define (setup)
  (size (* 10 N) (* 10 N)))
  

(define (draw)
  (set-title "Conway's Game of Life")
  (set! board (update))
  
  (background 255)
  (rect-mode 'corner)
  (stroke 0)
  (for ([i (in-range 1 (- N 1))])
    (for ([j (in-range 1 (- N 1))])
      (fill (if (live? i j) 0 255))
      (rect (* 10 j) (* 10 i) 10 10))))

