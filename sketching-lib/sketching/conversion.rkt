#lang racket
(provide binary unbinary int char)

(require racket/math
         "color.rkt")

; Converts an int, byte, char, or color to a String containing the equivalent binary notation.
(define (binary x)
  (define (pad s)
    (define n (string-length s))
    (if (< n 8)
        (string-append (make-string (- 8 n) #\0) s)
        s))
  (cond
    [(integer? x) (number->string x 2)]
    [(char?    x) (binary (char->integer x))]
    [(color?   x) (string-append (pad (binary (alpha x)))
                                 (pad (binary (red x))) 
                                 (pad (binary (green x)))
                                 (pad (binary (blue x))))]
    [else (error 'binary "can't convert value to binary, got: ~a" x)]))


(define (unbinary s)
  (string->number s 2))

(define (hex x)
  (define (pad s)
    (define n (string-length s))
    (if (< n 2)
        (string-append (make-string (- 2 n) #\0) s)
        s))
  (cond
    [(integer? x) (number->string x 16)]
    [(char?    x) (hex (char->integer x))]
    [(color?   x) (string-append (pad (hex (alpha x)))
                                 (pad (hex (red x))) 
                                 (pad (hex (green x)))
                                 (pad (hex (blue x))))]
    [else (error 'hex "can't convert value to hexadecimal, got: ~a" x)]))

(define (unhex s)
  (string->number s 2))

(define (int x)
  (cond
    [(number? x) (cond
                   [(inexact? x) (exact-floor x)]
                   [(exact? x)   (exact-floor x)]
                   [(integer? x) x]
                   [else (error 'int "got: ~a" x)])]
    [(char? x)    (char->integer x)]
    [(eq? x #t)   1]
    [(eq? x #f)   0]
    [else (error 'int "got: ~a" x)]))


(define (char x)
  (cond
    [(number? x) (integer->char (int x))]
    [else        (error 'char "got: ~a" x)]))
