#lang racket/base
(require racket/string)

(provide id-contains?
         stringify
         split-id
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
