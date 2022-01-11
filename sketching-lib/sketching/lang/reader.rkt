(module reader syntax/module-reader
  ; 1. Module path of the language.
  sketching/main
  ;   The module path `sketching/main` is used in the language position
  ;   of read modules. That is, reading `#lang sketching` will produce a
  ;   module with `sketching/main` as language.

  ; 2. Reader options (#:read, #:read-syntax, etc. ...)
  #:module-wrapper (λ (thunk) (adjust (thunk)))

  ; 3. Forms as in the body of racket/base 

  ; After standard reading, we will rewrite
  ;      id[expr ...]
  ; to
  ;      (#%ref id expr ...).

  ; We will use this to index to vectors, strings and hash tables.
  

  ; Since adjust is called after reading, we are essentially working with
  ; three passes.
  ;   - read-syntax
  ;   - adjust
  ;   - expand
  
  ; Let's define our `adjust` pass.

  (require racket/runtime-path racket/syntax
           (except-in syntax/parse char))
  
  (define (read-string str #:source-name [source-name #f])
    (define in (open-input-string str))
    ; (port-count-lines! in)
    (read-syntax source-name in))
  
  (define (adjust stx)
    (syntax-parse stx
      [(a . d) (adjust-dotted-list stx)]
      [_       stx]))
  
  (define (adjust-dotted-list stx)    
    (syntax-parse stx
      [(id:id (~and [e:expr ...] brackets)  . more)
       (cond
         [(and (eqv? (syntax-property #'brackets 'paren-shape) #\[)
               (= (+ (syntax-position #'id) (syntax-span #'id))
                  (syntax-position #'brackets)))
          (let ([adjusted-more (adjust #'more)]
                [arguments     (syntax->list #'(id e ...))])
            (datum->syntax #f
                           `((#%ref ,@arguments) . ,adjusted-more)
                           stx))]
         [else
          (with-syntax ([(_ . rest) stx])
            (let ([adjusted-rest (adjust-dotted-list #'rest)])
              (datum->syntax #f
                             `(,#'id . ,adjusted-rest)
                             stx)))])]
      [(a . more)
       (let ([adjusted-a    (adjust #'a)]
             [adjusted-more (adjust #'more)])
         (datum->syntax #f
                        `(,adjusted-a . ,adjusted-more)
                        stx))]
      [_
       (raise-syntax-error 'adjust-dotted-list "expected a dotted list" stx)]))

  ; > (displayln (adjust (read-string "(foo[bar])")))
  ; #<syntax:string::2 (ref (foo bar))>

  ; > (displayln (adjust (read-string "(foo [bar])")))
  ; #<syntax:string::1 (foo (bar))>

  ; > (displayln (adjust (read-string "(foo v[1] bar)")))
  ; #<syntax:string::1 (foo (#%ref v 1) bar)>

  )


