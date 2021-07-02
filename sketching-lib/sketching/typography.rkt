#lang racket/base
;;;
;;; Exports
;;;

(provide text/aligned)


;;;
;;; Imports
;;;

(require racket/draw
         racket/class
         racket/match
         racket/format
         racket/list
         racket/string
         ; "color.rkt" "math.rkt"
         "parameters.rkt")

;;;
;;; Contents
;;;

(define (contents? x)
  (or (string? x)
      (number? x)
      (and (list? x)
           (andmap contents? x))))


(define (contents->list x)
  (cond
    [(string? x) (list x)]
    [(number? x) (list (word (number->string x)))]
    [(list?   x) (append-map contents->list x)]
    [else
     (error 'contents->list (~a "expects contents, got: " x))]))

;;;
;;; Words
;;;

; Wrapping a string in a  word  structure tells the later stage
; of the pipeline, that the string is already a word - and doesn't
; need to be split at white spaces.

(struct word (string whitespace?) #:transparent)

(define (word-newline? w)
  (equal? "\n" (word-string w)))

(define (whitespace? c)
  (memv c '(#\space #\newline #\tab)))


(define (end-of-word s i n)
  (cond
    [(= i n)                         n]
    [(whitespace? (string-ref s i))  i]
    [else                            (end-of-word s (+ i 1) n)]))


(define (end-of-whitespace s i n)
  (cond
    [(= i n)                               n]
    [(eqv? (string-ref s i) #\newline)     i]
    [(not (whitespace? (string-ref s i)))  i]
    [else                                  (end-of-whitespace s (+ i 1) n)]))

(define (string-splitter s)
  (define n (string-length s))
  (if (= n 0) '() (do-string-splitter s 0 n)))


(define (do-string-splitter s i n)
  ; return a list of words in s[i..n].
  (cond
    [(= i n) '()]
    [else    (define c (string-ref s i))
             (if (whitespace? c)
                 (do-string-splitter-whitespace s i n)
                 (do-string-splitter-word       s i n))]))

(define (do-string-splitter-word s i n)
  ; we now know that s[i..n] begins with a word
  (define j (end-of-word s i n))
  (cond
    [(= i n) '()]
    [(= i j) (do-string-splitter-whitespace s j n)]
    [else    (cons (word (substring s i j) #f)
                   (do-string-splitter-whitespace s j n))]))

(define (do-string-splitter-whitespace s i n)
  ; we now know that s[i..n] begins with whitespace
  (define j (end-of-whitespace s i n))
  (cond
    [(= i n) '()]
    ; #\newline is always alone
    [(eqv? (string-ref s i) #\newline)
     (cons (word (substring s i (+ i 1)) #t)
           (do-string-splitter s (+ i 1) n))]
    [else
     (cons (word (substring s i j) #t)
           (do-string-splitter-word s j n))]))
                

(define (string->words s)
  ; Note: This throws the white space away.
  (cond
    [(word? s)   (list (word-string s))]
    [(string? s) (string-splitter s)]
    [else        (error 'string->word (~a "expected a word or a string, got: " s))]))

(define (strings->words xs)
  (append-map string->words xs))

;;;
;;; Dimensions
;;;

(struct dim (width height ascent descent))

(define (word-dimensions dc word)
  (define s (word-string word))
  (define-values (w h descent extra-vertical-space) (send dc get-text-extent s))
  (define ascent (- h descent))
  (dim w h ascent descent))

(define (char-width dc c)
  (define s (string c))
  (define-values (w h descent extra-vertical-space) (send dc get-text-extent s))
  w)

(define (char-height dc c)
  (define s (string c))
  (define-values (w h descent extra-vertical-space) (send dc get-text-extent s))
  h)


;;;
;;; Lines
;;;

(define (drop-whitespace-words words)
  (cond
    [(empty? words)                           '()]
    [(word-whitespace? (first (first words))) (drop-whitespace-words (rest words))]
    [else                                     words]))

(define (trim-line words)
  (reverse (drop-whitespace-words (reverse (drop-whitespace-words words)))))


(define (words->lines words dims text-width space-width)
  (unless text-width (set! text-width +inf.0))

  (map trim-line
  (let loop ([words      words]
             [dims       dims]
             [lines      '()]   ; processed lines (in reverse order)
             [line       '()]   ; current line    (in reverse order)
             [line-width 0])    ; width of the words in line
    (cond
      [(and (empty? words) (empty? line))
       (reverse lines)]
      [(empty? words)
       (reverse (cons (reverse line) lines))]
      [else
       (define word (first words))
       (define dim  (first dims))
       (define w    (dim-width dim))

       (define new-line-width
         (if (empty? line)
             w
             (+ line-width w)))
       
       (cond
         ; explicit newline in text
         [(word-newline? word)
          (loop (rest words) (rest dims)
                (cons (reverse line) lines)
                '()
                0)]
         ; the word fits on the current line, add to current
         [(< new-line-width text-width)
          (loop (rest words) (rest dims)
                lines
                (cons (list word dim) line)
                (+ line-width 0 #;space-width w))]
         ; the word doesn't fit, so we are ready for a new line,
         ; first the case, where the word doesn't fit on a single line
         [(> w text-width)
          (cond
            [(empty? line)
             (define new-line (list (list word dim)))             
             (loop (rest words) (rest dims) (cons new-line lines) '() 0)]
            [else
             (define new-line (list (list word dim)))
             (define cur-line (reverse line))
             (loop (rest words) (rest dims) (cons new-line (cons cur-line lines)) '() 0)])]
         ; second, the word doesn't fit, end current line and add word to next line
         ;         well if the word is whitespace, we skip it
         [(word-whitespace? word)
          (loop (rest words) (rest dims) (cons (reverse line) lines) '() 0)]
         [else
          (define next-line (list (list word dim)))
          (loop (rest words) (rest dims) (cons (reverse line) lines) next-line w)])]))))



(define (text/aligned contents dc x0 y0 width height alignment leading)
  ; (displayln (list 'text/aligned contents))
  ; contents        contents i.e. something that can be converted to text
  ; (x0,y0)         position of upper, left corner
  ; width, height   width and height of the rectangle in which to draw the text
  ; alignment       'left 'center 'right
  ; leading         vertical space between lines

  ; Notes:
  ;  width =#f      use as much horizontal space as needed (i.e. only one line)
  ;  height=#f      use as much vertical   space as needed 

  ; 0. Setup
  (define space-width  (char-width  dc #\space))
  (define space-height (char-height dc #\space))
  (define text-width   width)
  (define text-height  height)
  ;; (displayln (list 0 'space-width  space-width))
  ;; (displayln (list 0 'space-height space-height))
  ;; (displayln (list 0 'text-width   text-width))
  ;; (displayln (list 0 'text-height  text-height))
  

  ; 1. Convert the contents to a list of string
  (define strings (contents->list contents))
  ; (displayln (list 1 'strings strings))

  ; 2. Split each string into words.  
  (define words (strings->words strings))
  ; (displayln (list 2 'words words))

  ; 3. Determine the dimensions of each word.
  (define dims (map (λ (w) (word-dimensions dc w)) words))
  ; (displayln (list 3 'dims dims))

  ; 4. Form lines from the words.
  (define lines (words->lines words dims text-width space-width))
  ; (displayln (list 4 'lines lines))

  ; 5. Line widths and heights
  (define line-widths  (map (λ (line) (line-width line space-width))  lines))
  ; (displayln (list '5a 'line-widths line-widths))
  (define line-heights (map (λ (line) (line-height line space-height)) lines)) 
  ; (displayln (list '5b 'line-heights line-heights))

  ; 6. Draw the lines
  (define y y0)
  (for ([line lines] [w line-widths] [h line-heights]
       ; only draw text that fits completely inside the rectangle
        #:break (> (+ (- y y0) h) text-height))
    (case alignment
      [(left)   (draw-line/left   dc line w x0 y space-width text-width)]
      [(right)  (draw-line/right  dc line w x0 y space-width text-width)]
      [(center) (draw-line/center dc line w x0 y space-width text-width)]
      [else     (error 'text/aligned (~a "alignment was: " alignment))])
    (set! y (+ y h leading))))

(define (line-width line space-width)
  ; (displayln (list 'line-width line))
  ; a line is of the form:  (list (list string dim) ...)
  (define n (length line))
  (define total-word-width  (for/sum ([wd line]) (dim-width (second wd))))  
  total-word-width)

(define (line-height line space-height)
  (define heights (for/list ([wd line]) (dim-height (second wd))))
  (apply max (cons space-height heights)))


(define (draw-line/left dc line line-width x0 y0 space-width text-width)
  (define x x0)
  (for ([wd line])
    (define word (first  wd))
    (define dim  (second wd))
    (define s    (word-string word))
    (send dc draw-text s x y0)
    (set! x (+ x (dim-width dim)))))


(define (draw-line/right dc line line-width x0 y0 space-width text-width)
  (draw-line/left dc line line-width
                  (+ x0 (- text-width line-width))
                  y0
                  space-width text-width))


(define (draw-line/center dc line line-width x0 y0 space-width text-width)
  (draw-line/left dc line line-width
                  (+ x0 (* 0.5 (- text-width line-width)))
                  y0
                  space-width text-width))

  


