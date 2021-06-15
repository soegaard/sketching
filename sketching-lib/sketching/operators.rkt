#lang racket/base
(require
 racket/class
 ffi/unsafe/atomic)


#;(define (dc-mixin extend-backend%)
  (defclass* dc% extend-backend% (dc<%>)
    (inherit flush-cr get-cr release-cr release-unchanged-cr end-cr
             init-cr-matrix init-effective-matrix
	     get-pango
             install-color dc-adjust-smoothing get-hairline-width dc-adjust-cap-shape
             reset-clip
             collapse-bitmap-b&w?
             ok? can-mask-bitmap? get-clear-operator)

    
    (define-syntax-rule (with-cr* release-cr default cr . body)
      (begin
        (start-atomic)
        (let ([cr (get-cr)])
          (if cr 
              (begin0
                  (begin . body)
                (release-cr cr)
                (end-atomic))
              (begin
                (end-atomic)
                default)))))

    (define-syntax-rule (with-cr default cr . body)
      (with-cr* release-cr default cr . body))
    (define-syntax-rule (with-unchanged-cr default cr . body)
      (with-cr* release-unchanged-cr default cr . body))))

  


