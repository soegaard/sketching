#lang racket/base
(require racket/string)

(provide id-contains?
         stringify
         split-id
         split-id-at-dot/underscore
         number-id->number)

(define (number-id->number id)
    (define x (string->number (symbol->string (syntax-e id))))
  (if (integer? x) x id))

(define (stringify x #:mode [mode 'convert])
  (cond
    [(string? x)     x]
    [(char? x)       (string x)]
    [(symbol? x)     (symbol->string x)]
    [(number? x)     (number->string x)]      
    [(identifier? x) (symbol->string (syntax-e x))]
    [else            #f]))

(define (id-contains? id needle)
  (string-contains? (stringify id) (stringify needle)))

(define (index-of-char c str [start 0])
  (define n (string-length str))
  (and (< start n)
       (for/first ([i (in-range start n)]
                   #:when (char=? (string-ref str i) c))
         i)))

(define (index-of-dot        str [start 0]) (index-of-char #\. str start))
(define (index-of-underscore str [start 0]) (index-of-char #\_ str start))

(define (index-of-dot/underscore str [start 0])
  (define i (index-of-dot        str start))
  (define j (index-of-underscore str start))
  (or (and i j (min i j))
      i
      j))

(define (split-string-at-dot/underscore str [start 0])
  ; > (split-string-at-dot/underscore "foo.bar_baz.qux")
  ; '("foo" "." "bar" "_" "baz" "." "qux")
  (define i (index-of-dot/underscore str start))
  (cond
    [i (define pre  (substring str start i))
       (define sep  (string (string-ref str i)))
       (define post (split-string-at-dot/underscore str (+ i 1)))
       (list* pre sep post)]
    [else
       (list (substring str start (string-length str)))]))
      
(define (split-id-at-dot/underscore id [context id] [prop #f])
  (define str (symbol->string (syntax-e id)))
  (define strs (split-string-at-dot/underscore str))
  (define srcloc id)
  (for/list ([str strs])
    (define sym (string->symbol str))
    (datum->syntax context sym srcloc prop)))


(define (split-id id sep [context id] [prop #f])
  ; note: context is either #f or a syntax object
  ;       prop    is either #f or a syntax object
  (cond
    [(not (id-contains? id sep)) id]
    [else                        
     (define strs (string-split (stringify id) (stringify sep)))
     (define srcloc id)
     (for/list ([str strs])
       (define sym (string->symbol str))
       (datum->syntax context sym srcloc prop))]))

