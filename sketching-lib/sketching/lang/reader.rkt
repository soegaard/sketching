(module reader syntax/module-reader
  sketching/main)

;; #lang s-exp syntax/module-reader
;; racket/base
;; #:wrapper1 (lambda (thunk)
;;              (parameterize ([read-cdot #true])
;;                (thunk)))

