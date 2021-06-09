#lang racket/base
(require racket/match racket/list racket/class
         (for-syntax racket/base syntax/parse racket/syntax
                     racket/string))

(provide
 dot-field
 dot-assign-field
 declare-struct-fields
 app-dot-method
 define-method
 :=
 ;apply-dot-method
 ; get-dot-method
 ; define-method

 )


;;;
;;; Dot Fields and Methods
;;;

; For a struct Circle defined as:
;     (struct Circle (x y r))
; we would like to write
;     (define c (Circle 10 20 30))
;     (circle c.x c.y c.r)
; in order to draw the circle. Here  circle is the draw-a-circle
; primitive provided by Sketching.

; The value stored in c is a Circle struct, so we need
; associate Circle structs with a set of fields.

; (define-fields Circle Circle? (x y r))

; We can now write:
;    (define (draw-circle c)
;      (circle c.x c.y c.r))


; (here  draw  is called a method).

; To do that we use  define-method:

; (define-method Circle Circle? (draw color)
;   (stroke color)
;   (circle (Circle-x this) (Circle-y this) 


;; (define-method struct-name struct-predicate
;;   (method-name . formals) . more)



;;;
;;; Implementation
;;;

; Each "class" e.g. struct or vector is identified by an unique symbol,
; called the class symbol. 
; For (define-fields Circle Circle? (x y r)) the class symbol will be Circle.
; Each class has a list of fields, here the list of fields are x, y and z.

; Now at runtime  c.x  must called the accessor Circle-x,
; so we also need to store the accessor of each field.

; And later we want to support assignments of the form (:= c.x 10),
; so we also store mutators for the fields.

(struct info (accessor mutator))

; symbol -> (list (cons symbol info) ...)
(define class-symbol-to-fields+infos-ht     (make-hasheq))

(define (add-fields! class-symbol fields accessors mutators)
  ; todo: signal error, if class symbol is in use?
  (define fields+infos (map cons fields (map info accessors mutators)))
  (hash-set! class-symbol-to-fields+infos-ht class-symbol fields+infos))

(define (get-fields+infos class-symbol)
  (hash-ref class-symbol-to-fields+infos-ht class-symbol #f))

(define (find-info fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (cdr x))]
    [else                          #f]))

(define (find-accessor fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (info-accessor (cdr x)))]
    [else                          #f]))

(define (find-mutator fields+infos field)
  (cond
    [(assq field fields+infos) => (λ (x) (info-mutator (cdr x)))]
    [else                          #f]))


; In an expression like  c.x  the identifier c is bound to
; a value. To get from the value to the class symbol, we
; need some predicates.


(define class-symbol-to-predicate-ht     (make-hasheq)) ; symbol -> (value -> boolean)
(define predicate-to-class-symbol-assoc  '())           ; (list (cons predicate symbol) ...)


(define (add-predicate! class-symbol predicate)
  (hash-set! class-symbol-to-predicate-ht class-symbol predicate)
  (set! predicate-to-class-symbol-assoc
        (cons (cons predicate class-symbol)
              predicate-to-class-symbol-assoc)))

(define (find-class-symbol value)
  (let loop ([as predicate-to-class-symbol-assoc])
    (cond
      [(empty? as) #f]      ; the value has no associated class symbol
      [else        (define predicate+symbol (car as))
                   (define predicate        (car predicate+symbol))
                   (if (predicate value)
                       (cdr predicate+symbol)
                       (loop (cdr as)))])))

(define (find-class-symbol+predicate value)
  (let loop ([as predicate-to-class-symbol-assoc])
    (cond
      [(empty? as) (values #f #f)]  ; the value has no associated class symbol
      [else        (define predicate+symbol (car as))
                   (define predicate        (car predicate+symbol))
                   (if (predicate value)
                       (values (cdr predicate+symbol) predicate)
                       (loop (cdr as)))])))

; Finally we need address how  c.x  eventually evaluates (Circle-x c).
; Since  c.x  isn't bound in our program, the expander expands
; c.x into (#%top c.x). In "main.rkt" we have defined a
; #%sketching-top, which will be used as #%top by programs
; written in the Sketching language.

; In #%sketching-top we detect that the unbound identifier contains
; a dot, and we can then expand it into:
;   (dot-field c x)
; The naive implementation of  dot-field will expand into:

;; (let ()
;;   (define class-symbol     (find-class-symbol c))
;;   (define fields+accessors (get-fields+accessors class-symbol))
;;   (define accessor         (find-accessor fields+accessors 'x))
;;   (accessor c))

; However, if c.x is being used in a loop where c runs through a series of circles,
; then it is a waste of time to search for the same class symbol and accessor
; each time. Instead we can cache the accessor (and a predicate) so we can reuse
; the next time. The predicate is used to check that we got a value of the
; same time as last - if not, then we need to look for the new class.


(define-syntax (dot-field stx)
  (syntax-parse stx
    [(_dot-field object:id field:id)
     (with-syntax
       ([cached-accessor  (syntax-local-lift-expression #'#f)] ; think: (define cached-accessor #f) 
        [cached-predicate (syntax-local-lift-expression #'#f)] ;        is added at the top-level
        [stx stx])
       #'(let ()            
           (define accessor
             (cond
               [(and cached-predicate (cached-predicate object)) cached-accessor]
               [(object? object)
                (set! cached-accessor  (λ (obj) (get-field field obj)))
                ; (class-field-accessor class-expr field-id) ; todo !!
                (set! cached-predicate object?)
                cached-accessor]
               [else
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define fields+infos                    (get-fields+infos class-symbol))
                (define a                               (find-accessor fields+infos 'field))
                (set! cached-accessor  a)
                (set! cached-predicate predicate)
                (unless a
                  (raise-syntax-error 'dot-field "object does not have this field" #'stx))
                a]))
           (accessor object)))]
  [(_dot-field object:id field:id ... last-field:id)
   (syntax/loc stx
     (let ([t (dot-field object field ...)])
       (dot-field t last-field)))]))


(define-syntax (dot-assign-field stx)
  (syntax-parse stx
    [(_dot-assign-field object:id field:id e:expr)
     (with-syntax ([cached-mutator   (syntax-local-lift-expression #'#f)]
                   [cached-predicate (syntax-local-lift-expression #'#f)]
                   [stx stx])
       #'(let ()
           (define mutator
             (cond
               [(and cached-predicate (cached-predicate object)) cached-mutator]
               [(object? object)
                (set! cached-mutator  (λ (obj v) (set-field! field obj v)))
                (set! cached-predicate object?)
                cached-mutator]
               [else
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define fields+infos                    (get-fields+infos class-symbol))
                (define m                               (find-mutator fields+infos 'field))
                (set! cached-mutator  m)
                (set! cached-predicate predicate)
                m]))
           (mutator object e)))]
    [(_dot-field object:id field:id ... last-field:id e:expr)
     (syntax/loc stx
       (let ([w e] [t (dot-field object field ...)])
         (dot-assign-field t last-field w)))]))

;;;
;;; Defining class with fields
;;;

; We would like to write:
;     (declare-struct-fields Circle Circle? (x y r))
; to declare that the Circle "class" has fields x, y and r.

; The general form is:
;     (declare-struct-fields class-symbol predicate (field ...))

(define-syntax (declare-struct-fields stx)
  (syntax-parse stx
    [(_declare-struct-fields struct-name:id predicate:id (field:id ...))
     (with-syntax ([class-symbol #'struct-name]
                   [(accessor ...)
                    (for/list ([f (syntax->list #'(field ...))])
                      (format-id f "~a-~a" #'struct-name f))]
                   [(mutator ...)
                    (for/list ([f (syntax->list #'(field ...))])
                      (format-id f "set-~a-~a!" #'struct-name f))])
       (syntax/loc stx
         (begin
           (add-predicate! 'class-symbol predicate)
           (add-fields! 'class-symbol '(field ...)
                        (list accessor ...)
                        (list mutator  ...)))))]))
                               

;;;
;;; Assignment
;;;

;; (define str (symbol->string (syntax-e #'top-id)))
;; (cond
;;   [(string-contains? str ".")
;;    (define ids (map string->symbol (string-split str ".")))
;;    (with-syntax ([(id ...) (for/list ([id ids])
;;                              (datum->syntax #'top-id id))])
;;      #'(dot-field id ...))]
;;   [else
;;    #'(#%top . top-id)])

; (:= v.i.j 42)
; (vector-set! (vector-ref v i) j 42)


(define-syntax (:= stx)
  (syntax-parse stx
    [(_ x:id e)
     (define str (symbol->string (syntax-e #'x)))
     (cond
       [(string-contains? str ".")
        (define ids (map string->symbol (string-split str ".")))
        (with-syntax ([(x ...) (for/list ([id ids])
                                 (datum->syntax #'x id))])
            (syntax/loc stx                                 
              (dot-assign-field x ... e)))]
       [else
        (syntax/loc stx
          (set! x e))])]
    [(_ v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i e2)))))]))



;;;
;;; Methods
;;;

; For a struct Circle defined as:

;     (struct Circle (x y r))

; we would like to write

;     (define c (Circle 10 20 30))
;     (define-method Circle (draw this color)
;        (stroke color)
;        (circle this.x this.y this.r))
;     (c.draw "red")

; in order to draw the circle.

; The method draw of the class Circle is called as (c.draw "red")
; and turns into (Circle-draw c "red").

(struct minfo (procedure)) ; "method onfo"

; symbol -> (list (cons symbol minfo) ...)
(define class-symbol-to-methods+minfos-ht (make-hasheq))

(define (add-method! class-symbol method procedure)  
  (define methods+minfos (hash-ref class-symbol-to-methods+minfos-ht class-symbol '()))  
  (define method+minfo   (cons method (minfo procedure)))
  (hash-set! class-symbol-to-methods+minfos-ht class-symbol (cons method+minfo methods+minfos)))

(define (get-methods+minfos class-symbol)
  (hash-ref class-symbol-to-methods+minfos-ht class-symbol #f))

(define (find-method-procedure methods+minfos method)
  (cond
    [(assq method methods+minfos) => (λ (x) (minfo-procedure (cdr x)))]
    [else                            #f]))


(define-syntax (app-dot-method stx)
  (syntax-parse stx
    [(_app-dot-method (object:id method:id) . args)
     (with-syntax
       ([cached-procedure (syntax-local-lift-expression #'#f)] ; think: (define cached-procedure #f) 
        [cached-predicate (syntax-local-lift-expression #'#f)] ;        is added at the top-level
        [stx stx])
       #'(let ()
           (define procedure
             (cond
               [(and cached-predicate (cached-predicate object)) cached-procedure]
               [(object? object)
                ; XXX
                (set! cached-predicate object?)
                (set! cached-procedure (λ (obj . as)
                                         ;(displayln (list 'obj obj))
                                         ;(displayln (list 'args args))
                                         (send/apply obj method as)))
                cached-procedure]
               [else
                (define-values (class-symbol predicate) (find-class-symbol+predicate object))
                (define methods+minfos                  (get-methods+minfos class-symbol))
                (define p                               (find-method-procedure methods+minfos 'method))
                (set! cached-procedure p)
                (set! cached-predicate predicate)
                (unless p
                  (raise-syntax-error 'app-dot-methods "object does not have this method" #'stx))
                p]))
           (procedure object . args)))]
    [(_app-dot-field (object:id field:id ... method:id) . args)
     (syntax/loc stx
       (let ([obj (dot-field object field ...)])
         (app-dot-method (obj method) . args)))]))


(define-syntax (define-method stx)
  (syntax-parse stx
    [(_define-method struct-name:id (method-name:id . formals) . more)
     (with-syntax ([struct-predicate (format-id stx "~a?"   #'struct-name)]
                   [struct-method    (format-id stx "~a-~a" #'struct-name #'method-name)]
                   [this             (format-id stx "this")])
       (syntax/loc stx
         (begin
           (define (struct-method this . formals) . more)
           (add-predicate! 'struct-name struct-predicate)
           (add-method! 'struct-name 'method-name struct-method))))]))


;;;
;;; Builtin "classes"
;;;

(add-predicate! 'vector vector?)
(add-predicate! 'list   list?)
(add-predicate! 'string string?)

;;; Builtin fields for builtin "classes"
(add-fields! 'vector '(ref x y z)
             (list (λ (this) (λ (index) (vector-ref this index)))
                   (λ (v) (vector-ref v 0))
                   (λ (v) (vector-ref v 1))
                   (λ (v) (vector-ref v 2)))
             (list #f
                   (λ (v e) (vector-set! v 0 e))
                   (λ (v e) (vector-set! v 1 e))
                   (λ (v e) (vector-set! v 2 e))))

(add-fields! 'list '(x y z)
             (list first second third)
             (list #f    #f     #f))

(add-method! 'vector 'length vector-length)
(add-method! 'vector 'ref    vector-ref)
(add-method! 'vector 'list   vector->list)
(add-method! 'vector 'fill!  vector-fill!)
(add-method! 'vector 'values vector->values)

(add-method! 'list 'length  length)
(add-method! 'list 'ref     list-ref)
(add-method! 'list 'vector  list->vector)

(add-method! 'string 'length  string-length)
(add-method! 'string 'ref     string-ref)
(add-method! 'string 'list    string->list)




;; ;;;
;; ;;; Methods
;; ;;; 

;; (hash-set*! class-symbol-to-methods-ht
;;             'vector
;;             (make-methods-ht 'length vector-length
;;                              'ref    vector-ref
;;                              'list   vector->list
;;                              'fill!  vector-fill!
;;                              'values vector->values)
;;             'list
;;             (make-methods-ht 'length length
;;                              'ref    list-ref
;;                              'vector list->vector
;;                              'values (λ (xs) (apply values xs)))
;;             'string
;;             (make-methods-ht 'length string-length
;;                              'ref    string-ref
;;                              'list   string->list)
;;             ; unknown class
;;             #f (make-hasheq))



;; (define class-symbol-to-methods-ht    (make-hasheq))

;; (define (make-methods-ht . args)
;;   (define key-methods
;;     (let loop ([args args])
;;       (match args
;;         [(list* key method more)
;;          (cons (cons key method)
;;                (loop more))]
;;         ['()
;;          '()])))
;;   (make-hasheq key-methods))




;; (define (predicates->symbol x)
;;  ...)  

;; (define (value->class-symbol v)
;;   (cond
;;     [(vector? v) 'vector]
;;     [(list?   v) 'list]
;;     [(string? v) 'string]
    
;;     [else        #f]))

;; (define-syntax (apply-dot-method stx)
;;   (syntax-parse stx
;;     [(_get object:id method-name:id more ...)
;;      #'(let ()
;;          (define class   (value->class-symbol object))         
;;          (define methods (hash-ref class-symbol-to-methods-ht class #f))
;;          (define method  (hash-ref methods 'method-name ))         
;;          (method object more ...))]))


;; (define-syntax (get-dot-method stx)
;;   (syntax-parse stx
;;     [(_get object:id method-name:id)
;;      #'(let ()
;;          (define cached-method    #f)
;;          (define cached-predicate #f)
;;          (λ args
;;            (define method
;;              (cond
;;                [(and cached-predicate (cached-predicate object)) cached-method]
;;                [else
;;                 (define class (value->class-symbol object))
;;                 (displayln (list 'class class))
;;                 (define p     (hash-ref class-symbol-to-predicates-ht class #f))
;;                 (displayln (list 'p p))
;;                 (define ms    (hash-ref class-symbol-to-methods-ht    class #f))
;;                 (displayln (list 'ms ms))
;;                 (define m     (hash-ref ms 'method-name))
;;                 (displayln (list 'm m))
;;                 (set! cached-predicate p)
;;                 (set! cached-method m)
;;                 m]))
;;            (apply method (cons object args))))]))


;; (define-syntax (define-method stx)
;;   (syntax-parse stx
;;     [(_define-method struct-name:id struct-predicate:id
;;         (method-name:id . formals)
;;         . more)
;;      (with-syntax ([struct-method (format-id stx "~a-~a" #'struct-name #'method-name)])
;;        (syntax/loc stx
;;          (begin
;;            (define (struct-method this . formals) . more)
;;            (hash-set*! class-symbol-to-predicates-ht 'struct-name struct-predicate)
;;            (define ht (hash-ref class-symbol-to-methods-ht 'struct-name #f))
;;            (unless ht
;;              (define new-ht (make-hasheq))
;;              (hash-set! class-symbol-to-methods-ht 'struct-name new-ht)
;;              (set! ht new-ht))
;;            (hash-set! ht 'method-name struct-method)
;;            (displayln " -- ")
;;            (displayln class-symbol-to-methods-ht)
;;            (newline)
;;            (displayln ht) (newline)
           
;;            )))]))
     


;; ;; (define-syntax (#%top stx)
;; ;;   (display ".")
;; ;;   (syntax-parse stx
;; ;;     [(_ . s:id) #'id]))
  

;; ;; (struct Circle (x y r c))

;; ;; ;; (define-method Circle (draw C [color "red"])
;; ;; ;;   (fill color)
;; ;; ;;   (circle C.x C.y C.r))
  

;; ;; ; (obj.name foo bar) => (mehthod foo

;; ;; (define c (Circle 10 20 30 "white"))

;; ;c.x
