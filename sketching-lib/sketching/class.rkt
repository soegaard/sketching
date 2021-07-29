#lang racket/base
(require racket/class)

(provide Class Object
         ; from racket/class:
         ; make-object new define/public define/override super-make-object inherit-field init-fields
         (all-from-out racket/class))

;;;
;;; Simple Classes and Objects for Sketching
;;;

; - all fields are public init-fields
; - all objects are printable

(require (for-syntax syntax/parse racket/base racket/syntax))

(define Object
  (class* object% (printable<%>)
    (super-new)
    (inspect (make-inspector))
    (define/public (custom-print     port qq-depth) (do-print this print   port))
    (define/public (custom-display   port)          (do-print this display port))
    (define/public (custom-write     port)          (do-print this write   port))

    (define (do-print object out port)
      (define-values (class skipped?) (object-info this))
      (define-values (class-name field-cnt field-name-list field-accessor
                                 field-mutator super-class skipped?-)
        (class-info class))
      (display "<object:"                  port)
      (display (symbol->string class-name) port)
      (for ([field field-name-list])
        (display " ("                         port)
        (display (symbol->string field)       port)
        (display " "                          port)
        (out (dynamic-get-field field object) port)
        (display ")"                          port))
      (display ">"                         port))))

(define-syntax (Class stx)
  (syntax-parse stx 
    [(_Class name:id super-class:id . body)
     (syntax/loc stx       
       (define name
         (class* super-class ()           
           (inspect (make-inspector)) ; needed in order to support printing
           . body)))]))
