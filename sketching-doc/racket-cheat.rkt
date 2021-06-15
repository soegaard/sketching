#lang racket/base
(require racket/list
         racket/match
         racket/runtime-path
         scribble/core
         scribble/base
         scribble/html-properties)

(struct *section (which label groups))
(struct *group (label rows))
(struct *row (label content))

(define how-many-sections (box 0))
(define rsections (box empty))

(define (CSection #:which which label . groups)
  (set-box! rsections
            (cons (*section which label groups)
                  (unbox rsections)))
  (set-box! how-many-sections
            (add1 (unbox how-many-sections))))
(define (CGroup label . rows)
  (*group label rows))
(define (CRow label content)
  (*row label content))

(define-runtime-path racket-cheat.css-path "racket-cheat.css")

(define cheat-style
  (style #f (list (html-defaults #"<!DOCTYPE html>" #"" '()))))

(define (render-cheat-sheet)
  (define left-col '())
  (define right-col '())
  (for ([s (in-list (unbox rsections))])
    (if (eq? 'left (*section-which s))
        (set! left-col (cons s left-col))
        (set! right-col (cons s right-col))))

  (element
   (style #f (list (alt-tag "div")
                   (attributes '([class . "Csheet"]))
                   (css-style-addition racket-cheat.css-path)))
   (list
    (render-column "Cleft" left-col)
    (render-column "Cright" right-col))))

(define (render-column id secs)
  (element (style #f (list (alt-tag "div")
                           (attributes `([id . ,id]
                                         [class . "Ccolumn"]))))
           (list (map render-section secs))))

(define (render-section s)
  (match-define (*section _ label gs) s)
  (element (style #f (list (alt-tag "div")
                           (attributes '([class . "Csection"]))))
           (cons
            (toc-target-element (style #f (list (alt-tag "h1"))) label
                                `(section ,(content->string label)))
            (map render-group gs))))

(define (render-group g)
  (match-define (*group label rs) g)
  (define more
    (element (style #f (list (alt-tag "table")))
             (map render-row rs)))
  (element (style #f (list (alt-tag "div")
                           (attributes '([class . "Cgroup"]))))
           (if label
               (list (element (style #f (list (alt-tag "h2"))) label)
                     more)
               more)))

(define (render-row r)
  (match-define (*row label content) r)
  (element (style #f (list (alt-tag "tr")))
           (list (element (style #f (list (alt-tag "td")))
                          (list label))
                 (element (style #f (list (alt-tag "td")))
                          content))))

(define MORE
  (element (style #f (list (hover-property "Click on the previous
function to see similar functions in the documentation.")))
           "..."))
(define LB (linebreak))

(provide cheat-style CSection CGroup CRow render-cheat-sheet MORE LB)
