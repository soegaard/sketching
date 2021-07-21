#lang scribble/manual
@;(require racket/gui/base)
@; The following command will build the manual and open it in a browser.
@; raco scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-sketching.scrbl && open html/manual-sketching.html
@(require racket/sandbox racket/format racket/file racket/runtime-path racket/string racket/list) 
@(require scribble/example)
@(require (for-syntax racket/base syntax/parse))
@(require (for-label ; (except-in racket/base #%app #%top)
           (except-in racket
                      #%app #%top #%module-begin class ; delay
                      second struct random round)
           sketching/exports-no-gui ; was sketching
           ))


@; Used to reference other manuals.
@(define reference.scrbl '(lib "scribblings/reference/reference.scrbl"))
@(define math.scrbl      '(lib "math/scribblings/math.scrbl"))

@; only id is linked with @racket[] the arguments are not

@(define-syntax (racketusage stx)
   (syntax-parse stx
     [(_ (id arg ...))
      (syntax/loc stx
        @racket[#:escape esc (id (esc @racketid[arg]) ...)])]
     [(_ other)
      (syntax/loc stx
        @racket[other])]))

@; Long urls

@(define (wikipedia name . preflow)
   (define url (string-append "https://en.wikipedia.org/wiki/" name))
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define (wikipedia/section url . preflow)
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define license-link
   @hyperlink["https://creativecommons.org/licenses/by-nc-sa/4.0/"
              "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."])

@(define sketching-github
   @hyperlink["https://github.com/soegaard/sketching/" "https://github.com/soegaard/sketching/"])

@(define sketching-manual-examples-github
   @hyperlink["https://github.com/soegaard/sketching/tree/main/sketching-examples/manual-examples/"
              "https://github.com/soegaard/sketching/tree/main/sketching-examples/manual-examples/"])


@(begin
   (define-runtime-path example-folder "manual-examples/")
   (define (example->path example) (build-path example-folder example))
   (define (example->string path) (file->string (example->path path)))
   ;; The examples urls to processing are so long, that it breaks printing
   ;; the web-page. We are therefore filtering out the links from the examples,
   ;; and use them instead in the text above.
   (define p-url "(https://github.com/processing/processing-docs/[^ \n]*)")
   (define r-url "(https://raw.githubusercontent.com/processing/processing-docs/[^ \n]*)")
   (define s-url "https://github.com/soegaard/sketching/tree/main/sketching-examples/manual-examples/")
   (define url-found #f)
   (define (contains-long-url? s)
     (define result (or (regexp-match p-url s) (regexp-match r-url s)))
     (when result (set! url-found (first result)))
     (and result #t))
   (define (original-name url)
     (define result (regexp-match "([^/]*[.]pde)" url))
     (and result (first result)))
   (define (example-from-file example)
     (define gh-url (string-append s-url example))
     (set! url-found #f)
     (define in (open-input-file (example->path example)))     
     (define code (codeblock
                   (string-append*
                    (add-between
                     (for/list ([line (in-lines in)] #:unless (contains-long-url? line))
                       line)
                     "\n"))))
     @(list code
            @hyperlink[gh-url (list @bold{Github: } " " example)]
            @linebreak[]
            (if (and url-found (original-name url-found))
                @hyperlink[url-found (list @bold{Original: } " " (original-name url-found))]
                '()))))
                



@title[#:tag "sketching"]{Sketching @linebreak[] A Language for Creative Coding}

@defmodule[sketching #:use-sources (sketching/exports-no-gui)]

@(void "Scribble follows require chains and uses the first
(re-)providing module it finds that documents a binding.  

On the documenting-bindings side, the defmodule form has a
#:use-sources option to list preferred modules to counts as ones
providing a binding (in cases where those modules are in the providing
chain). When documenting the sketching module, you may need to specify
the exports-no-gui module as the preferred source in that sense. Then,
a traversal from sketching-no-gui will hit the provider
exports-no-gui. (Or something like that.)")


Sketching is a language/library for creative coding. The focus is to make
graphical programs accessible for beginners, artists, educators and designers.
Sketching is free to use and the source is available to read and improve.

The main focus of Sketching is graphical programs. Running a program
will display a canvas ready to show static images or animation.

The inspiration for Sketching came from the Processing project.
Processing is a programming language built on top of Java.
The Processing language was devised by Ben Fry and Casey Reas.

Think of Sketching as what Processing would have looked like, if
it used Racket as its programming language instead of Java.
Alternatively, think of Sketching as Racket with an easy to use graphics library.

Although inspired by the Processing project, this project has no
affiliation with the Processing Foundation.

The reference documentation is licensed under @|license-link|.
This is the same license as the reference documentation for Processing uses.


@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@;local-table-of-contents[]

@(require "racket-cheat.rkt")

@section[#:tag "overview"]{Overview}

This manual consist of 3 sections. This first section contains a "cheat sheet"
that makes it easy to find where a function is documented. The second section
is the reference manual, where each function is described in details. The reference
section contains small examples. The third section contains more elaborate examples.


@(CSection
  #:which 'left
  "Color"
  (CGroup
   #f
   (CRow "Setting"
         @elem{
               @racket[stroke] @racket[no-stroke] @LB
               @racket[fill]   @racket[no-fill]   @LB
               @;racket[clear]
               @racket[background]
               @racket[color-mode]               
               })
   (CRow "Creating and Reading"
         @elem{@racket[color]
               @racket[red]
               @racket[green]
               @racket[blue]
               @racket[alpha]
               @LB
               @racket[hue]               
               @racket[saturation]
               @racket[brightness]
               @LB
               @racket[lerp-color]})))

@(CSection
  #:which 'left
  "Input"
  (CGroup
   #f
   (CRow "Coordinates"
         @elem{@racket[mouse-x]
               @racket[mouse-y]
               @racket[pmouse-x]
               @racket[pmouse-y]})
   (CRow "Buttons"
         @elem{@racket[mouse-button]
               @racket[mouse-pressed]
               @racket[mouse-released]})
   (CRow "Events"
         @elem{@racket[on-mouse-dragged]
               @racket[on-mouse-moved] @LB
               @racket[on-mouse-pressed]
               @racket[on-mouse-released]})

   (CRow "Keys"
         @elem{@racket[key] 
               @racket[key-pressed]})
   (CRow "Keyboard Events"
         @elem{@racket[on-key-pressed]
               @racket[on-key-released] @LB
               @racket[on-key-typed]})
   
   (CRow "Time and Date"
         @elem{@racket[year]
               @racket[month]
               @racket[day]
               @racket[hour]
               @racket[minute]
               @racket[second]               
               @racket[millis]})))



@(CSection
  #:which 'right
  "Shape"
  (CGroup
   #f
   (CRow "2D Primitives"
         @elem{@racket[arc]
               @racket[circle]
               @racket[ellipse]
               @racket[line]
               @racket[point]               
               @racket[quad]
               @racket[rect]
               @racket[square]
               @racket[triangle]}))
  (CGroup
   #f
   (CRow "Curves"
         @elem{@racket[bezier]}))
  (CGroup
   #f
   (CRow "Modes"
         @elem{@racket[ellipse-mode]
               @racket[rect-mode]})
   (CRow "Stroke"
         @elem{@racket[stroke-cap]
               @racket[stroke-join]
               @racket[stroke-weight]}))
  (CGroup
   #f
   (CRow "Vertex"
         @elem{@racket[vertex]
               @racket[begin-shape]               
               @racket[end-shape]})))

@(CSection
  #:which 'right
  "Image"
  (CGroup
   #f
   (CRow "Image"
         @elem{@racket[image] 
               @LB
               @racket[create-image]
               @racket[load-image]
               @racket[save-image]
               @LB
               @racket[image-mode]
               @LB
               @racket[load-pixels]
               @racket[set-pixel]
               @racket[update-pixels]               
               })))

@(CSection
  #:which 'right
  "Typography"
  (CGroup
   #f
   (CRow "Displaying"
         @elem{@racket[text]})
   (CRow "Attributes"
         @elem{@racket[text-align]
               @racket[text-size]
               @racket[text-face] 
               @LB
               @racket[text-family]
               @racket[text-weight]
               @racket[text-underlined?]
               @LB
               @racket[text-smoothing]
               @racket[text-size-in-pixels?]
               @racket[text-hinting]})))


@(CSection
  #:which 'right
  "Transform"
  (CGroup
   #f
   (CRow "Transform"
         @elem{@racket[rotate]
               @racket[scale]
               @racket[translate]
               @LB
               @racket[shear-x]
               @racket[shear-y]
               @LB
               @racket[push-matrix]
               @racket[pop-matrix]})))

@(CSection
  #:which 'right
  "Math"
  (CGroup
   #f
   (CRow "Operators"
         @elem{@racket[modulo]
               @racket[remainder]
               @racket[quotient]
               @LB
               @racket[+=] @racket[-=]
               @racket[*=] @racket[/=]
               @racket[++] @racket[--]})
   (CRow "Calculation"
         @elem{@racket[abs]
               @racket[ceil]
               @racket[constrain]
               @racket[dist]
               @racket[exp]
               @racket[floor]
               @racket[lerp]
               @racket[log]
               @racket[mag]
               @racket[remap]
               @racket[max]
               @racket[min]
               @racket[norm]
               @racket[pow]
               @racket[round]
               @racket[sq]
               @racket[sqrt]})
   (CRow "Trigonometry"
         @elem{@racket[cos]
               @racket[sin]
               @racket[tan]
               @racket[acos]
               @racket[asin]
               @racket[atan]
               @racket[atan2]
               @LB
               @racket[radians]
               @racket[degrees]})
   (CRow "Conversion"
         @elem{@racket[int]
               @racket[char]
               @racket[binary]
               @racket[unbinary]
               @racket[hex]
               @racket[unhex]})
   (CRow "Constants"
         @elem{@racket[pi]
               @racket[π]
               @racket[pi/2]
               @racket[π/2]
               @racket[pi/4]
               @racket[π/4]
               @racket[2pi]
               @racket[2π]
               })))


@(CSection
  #:which 'left
  "Environment"
  (CGroup
   #f
   (CRow "Size"
         @elem{@racket[size] @racket[width] @racket[height] @racket[fullscreen]})
   (CRow "Smoothing"
         @elem{@racket[smoothing] @racket[no-smooth]})
   (CRow "Frames"
         @elem{@racket[frame-count] @racket[frame-rate] @racket[set-frame-rate!]})
   (CRow "Mouse"
         @elem{@racket[cursor] @racket[no-cursor] @racket[focused]})
   (CRow "Other"
         @elem{@racket[nap]})))


@(CSection
  #:which 'left
  "Other"
  (CGroup
   #f
   (CRow "Loop"
         @elem{@racket[loop] @racket[no-loop]})
   (CRow "Assignment"
         @elem{@racket[:=]})
   (CRow "Structures"
         @elem{@racket[struct]})
   (CRow "Classes and objects"
         @elem{@racket[class] @racket[make-object] @racket[new]})
   (CRow "Dot Notation"
         @elem{. (dot notation)})))

@(render-cheat-sheet)

@;-------------------
@;-   REFERENCE
@;-------------------


@section{Reference}

This section is the reference manual of the Sketching programming language.
It contains a description of all available functions illustrated with plenty
small examples. Note that the function names in the source examples
are clickable. For more elaborate examples see the examples in the last
section of this manual.

If you find any mistakes in either the text or the examples, please
make an Github issue at @|sketching-github|.


@local-table-of-contents[#:style 'immediate-only]


@(render-cheat-sheet)

@subsection[#:tag "ref:color"]{Color}

@; Note: The documentation won't build on the package server if racket/gui is instantiated.
@;       This means we need to use racket/draw and sketching/exports-no-gui
@;       instead of racket/gui and sketching.
@(define draw-namespace (make-base-namespace))
@(define factory        (make-base-eval-factory (list 'racket/draw)))
@(define (make-sketching-eval)   
   (let ([e (factory)])
     (e '(require sketching/exports-no-gui sketching/parameters racket/draw))
     (e '(define (new-bitmap-dc w h)
           (define dc (new bitmap-dc% [bitmap (make-bitmap w h)]))
           (send dc set-smoothing 'aligned)
           dc))
     (e '(current-dc (new-bitmap-dc 100 100)))
     e))
@(define se (make-sketching-eval))

@;---------

@subsubsection{background}

@bold{Name: } @defidentifier[#'background]

Sets the background color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
   (eval:alts        (background 51)
              (begin (background 51)
                     (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
   (eval:alts        (background 255 204 0)
              (begin (background 255 204 0)
                     (send dc get-bitmap)))]



@bold{Usage}



@racketusage[(background rgb)]            @linebreak[]
@racketusage[(background rgb alpha)]      @linebreak[]
@racketusage[(background gray)]           @linebreak[]
@racketusage[(background gray alpha)]     @linebreak[]
@racketusage[(background v1 v2 v3)]       @linebreak[]
@racketusage[(background v1 v2 v3 alpha)] @linebreak[]
@racketusage[(background image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value.")
               (list @racketid[alpha] "An alpha level.")
               (list @racketid[gray]  "An integer.")
               (list @racketid[v1]    "Red or hue value.")
               (list @racketid[v2]    "Green or saturation value.")
               (list @racketid[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are hsb-values.


@bold{Description}

Sets the background color.

The @racket[background] function sets the color used for the background of
the Sketching window. The default background is light gray. This
function is typically used within @racket[draw] to clear the display window
at the beginning of each frame, but it can be used inside @racket[setup] to
set the background on the first frame of animation or if the backgound
need only be set once.

@(void "An image can also be used as the background for a sketch, although the
image's width and height must match that of the sketch window. Images
used with background() will ignore the current tint() setting. To
resize an image to the size of the sketch window, use
image.resize(width, height).")

@(void "It is not possible to use the transparency alpha parameter with
background colors on the main drawing surface. It can only be used
along with a PGraphics object and createGraphics().")


@;---------

@subsubsection{red}

@bold{Name: } @defidentifier[#'red]

Extracts the red component of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'rgb 255)
 (define c (color 255 204 0))
 (red c)
 (color-mode 'rgb 100)
 (red c)]


@examples[#:label #f #:eval se #:no-prompt
          (color-mode 'rgb 255)
          (fill c)          
          (rect 15 20  35 60)
          (fill (red c) 0 0)          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(red rgb)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Extracts the red value from a color.

The red component is a value from 0 to the maximum red level.
A red value of 0 means no red and the maximum means fully red.
The default range of red values is 0 to 255. Use @racket[color-mode]
to change the default range.

@;---------

@subsubsection{green}

@bold{Name: } @defidentifier[#'green]

Extracts the green component of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'rgb 255)
 (define c (color 20 75 200))
 (green c)
 (color-mode 'rgb 100)
 (green c)]


@examples[#:label #f #:eval se #:no-prompt
          (color-mode 'rgb 255)
          (fill c)          
          (rect 15 20  35 60)
          (fill 0 (green c) 0)          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(green rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Extracts the green value from a color.

The green component is a value from 0 to the maximum green level.
A green value of 0 means no green and the maximum means fully green.
The default range of green values is 0 to 255. Use @racket[color-mode]
to change the default range.

@;---------

@subsubsection{blue}

@bold{Name: } @defidentifier[#'blue]

Extracts the blue component of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'rgb 255)
 (define c (color 175 100 220))
 (blue c)
 (color-mode 'rgb 100)
 (blue c)]


@examples[#:label #f #:eval se #:no-prompt
          (color-mode 'rgb 255)
          (fill c)          
          (rect 15 20  35 60)
          (fill 0 0 (blue c))          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(blue rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Extracts the blue value from a color.

The blue component is a value from 0 to the maximum blue level.
A blue value of 0 means no blue and the maximum means fully blue.
The default range of blue values is 0 to 255. Use @racket[color-mode]
to change the default range.

@;---------


@subsubsection{alpha}

@bold{Name: } @defidentifier[#'alpha]

Extracts the alpha component of the color @racketid[c].

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 255) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (define c (color 0 126 255 102))
 (alpha c)]

@examples[#:label #f #:eval se #:no-prompt
          (define c (color 0 126 255 102))
          (no-stroke)
          (fill c)
          (rect 15 15  35 70)
          
          (fill (alpha c))
          (eval:alts (rect 50 15  35 70)
                     (begin (rect 50 15  35 70) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(alpha rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Extracts the alpha value from a color.

The alpha component is a value from 0 to the maximum alpha level.
An alpha value of 0 means fully transparent and the maximum means opaque.
The default range of alpha values is 0 to 255. Use @racket[color-mode]
to change the default range.


@;---------

@subsubsection{hue}

@bold{Name: } @defidentifier[#'hue]

Computes the hue value of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'hsb 255)
 (define c (color 0 126 255))
 (hue c)]


@examples[#:label #f #:eval se #:no-prompt
          (fill c)          
          (rect 15 20  35 60)
          (fill (hue c))          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))]

@examples[#:eval se #:no-prompt #:label #f
          (no-stroke)
          (color-mode 'hsb 360 100 100)
          (for ([x 360])
            (stroke x 100 100)
            (line x 0 x 50))
          (eval:alts "Hues from 0 to 360."
                     (send dc get-bitmap))]

@bold{Usage}

@racketusage[(hue rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Computes the hue value of a color.

The hue component is a value from 0 to the maximum hue level.
The color order is: red yellow green cyan blue purple (red again).
It is common to set the maximum hue level to 360 as people
often think of hue levels as degrees from 0 to 360 thus placing
the colors on a circle. Use @racket[color-mode] to change the default range.


@;---------

@subsubsection{saturation}

@bold{Name: } @defidentifier[#'saturation]

Computes the saturation value of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'hsb 255)
 (define c (color 0 126 255))
 (saturation c)]


@examples[#:label #f #:eval se #:no-prompt
          (fill c)          
          (rect 15 20  35 60)
          (fill (saturation c))          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))]

@examples[#:eval se #:no-prompt #:label #f
          (no-stroke)
          (color-mode 'hsb 360 100 100)
          (for ([x 100])
            (stroke 0 x 100)
            (line x 0 x 50))
          (eval:alts "Red with saturation levels from from 0 to 100 percent."
                     (send dc get-bitmap))]

@bold{Usage}

@racketusage[(saturation rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Computes the saturation value of a color.

The saturation component is a value from 0 to the maximum saturation level.
Often the maximum saturation level is set to 1. or 100 as one tends
to think of saturation levels as percentages. Use @racket[color-mode]
to change the default range.


@;---------

@subsubsection{brightness}

@bold{Name: } @defidentifier[#'brightness]

Computes the brightness value of a color.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
 (color-mode 'hsb 255)
 (define c (color 0 126 255))
 (brightness c)]


@examples[#:label #f #:eval se #:no-prompt
          (fill c)          
          (rect 15 20  35 60)
          (fill (brightness c))          
          (eval:alts        (rect 50 20  35 60)
                     (begin (rect 50 20  35 60) (send dc get-bitmap)))]


@examples[#:hidden #:eval se
          (current-dc (new bitmap-dc% [bitmap (make-bitmap 100 50)]))]

@examples[#:eval se #:no-prompt #:label #f
          (no-stroke)
          (color-mode 'hsb 360 100 100)
          (for ([x 100])
            (stroke 0 100 x)
            (line x 0 x 50))
          (eval:alts "Red with brightness levels from from 0 to 100 percent."
                     (send dc get-bitmap))]

@bold{Usage}

@racketusage[(brightness rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value."))]


@bold{Description}

Computes the brightness value of a color.

The brightness component is a value from 0 to the maximum brightness level.
Often the maximum brightness level is set to 1. or 100 as one tends
to think of brightness levels as percentages. Use @racket[color-mode]
to change the default range.


@;---------

@subsubsection{fill}

@bold{Name: } @defidentifier[#'fill]

Sets the color used to fill shapes.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill 153)
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill 204 102 0)
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill "#ccffaa")
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(fill rgb)]            @linebreak[]
@racketusage[(fill rgb alpha)]      @linebreak[]
@racketusage[(fill gray)]           @linebreak[]
@racketusage[(fill gray alpha)]     @linebreak[]
@racketusage[(fill v1 v2 v3)]       @linebreak[]
@racketusage[(fill v1 v2 v3 alpha)] @linebreak[]
@racketusage[(fill image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value.")
               (list @racketid[alpha] "An alpha level.")
               (list @racketid[gray]  "An integer.")
               (list @racketid[v1]    "Red or hue value.")
               (list @racketid[v2]    "Green or saturation value.")
               (list @racketid[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are hsb-values.


@bold{Description}

Sets the color used to fill shapes. For example, if you run @racket[(fill 204 102 0)],
all subsequent shapes will be filled with orange. This color
is either specified in terms of the RGB or HSB color depending on the
current @racket[color-mode]. The default color space is RGB, with each value
in the range from 0 to 255.

When using hexadecimal notation to specify a color, use a string beginning with @racket["#"]
(e.g., @racket["#ccffaa"] or @racket["#44ccffaa"]).
If six digits are used to specify a color (just as colors are typically specified in HTML and CSS).
The alpha level is then set to maximum.

If eight digits are used, the first two characters define the alpha component,
and the remainder define the red, green, and blue components.

The value for the "gray" parameter must be less than or equal to the
current maximum value as specified by @racket[color-mode]. The default maximum
value is 255.

To change the color of the outline, use @racket[stroke].

To change the color of an image, use @racket[tint].

Use @racket[no-fill] to turn off filling.

@;---------

@subsubsection{stroke}

@bold{Name: } @defidentifier[#'stroke]

Sets the color used to draw lines and borders around shapes.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (stroke 153)
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (stroke 204 102 0)
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (stroke "#ccffaa")
          (eval:alts        (rect 15 20  70 60)
                     (begin (rect 15 20  70 60) (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(stroke rgb)]            @linebreak[]
@racketusage[(stroke rgb alpha)]      @linebreak[]
@racketusage[(stroke gray)]           @linebreak[]
@racketusage[(stroke gray alpha)]     @linebreak[]
@racketusage[(stroke v1 v2 v3)]       @linebreak[]
@racketusage[(stroke v1 v2 v3 alpha)] @linebreak[]
@racketusage[(stroke image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[rgb]   "A color value.")
               (list @racketid[alpha] "An alpha level.")
               (list @racketid[gray]  "An integer.")
               (list @racketid[v1]    "Red or hue value.")
               (list @racketid[v2]    "Green or saturation value.")
               (list @racketid[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racketid[v1], @racketid[v2], @racketid[v3] are hsb-values.


@bold{Description}


Sets the color used to draw lines and borders around shapes.
For example, after @racket[(stroke 204 102 0)], all subsequent shapes will be filled with orange.
This color is either specified in terms of the RGB or HSB color depending on the
current @racket[color-mode]. The default color space is RGB, with each value
in the range from 0 to 255.

When using hexadecimal notation to specify a color, use a string beginning with @racket["#"]
(e.g., @racket["#ccffaa"] or @racket["#44ccffaa"]).
If six digits are used to specify a color (just as colors are typically specified in HTML and CSS).
The alpha level is set to maximum.

If eight digits are used, the first two characters define the alpha component,
and the remainder define the red, green, and blue components.

The value for the "gray" parameter must be less than or equal to the
current maximum value as specified by @racket[color-mode]. The default maximum
value is 255.

To change the color inside a shape, use @racket[fill].

Use @racket[no-stroke] to turn off outline drawing.


@;---------

@subsubsection{no-fill}

@bold{Name: } @defidentifier[#'no-fill]

Disables filling of shapes.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill 255)
          (rect 15 20  55 55)
          (no-fill)
          (eval:alts        (rect 30 35  55 55)
                     (begin (rect 30 35  55 55) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(no-fill)]            @linebreak[]


@bold{Description}

Disables filling of shapes. If both @racket[no-stroke] and @racket[no-fill] are called, nothing will be drawn to the screen.


@;---------

@subsubsection{no-stroke}

@bold{Name: } @defidentifier[#'no-stroke]

Disables drawing the stroke (outline). 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill 255)
          (no-stroke)
          (eval:alts        (rect 20 20  60 60)
                     (begin (rect 20 20  60 60) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(no-stroke)]            @linebreak[]


@bold{Description}

Disables drawing the stroke (outline).
If both @racket[no-stroke] and @racket[no-fill] are called, nothing will be drawn to the screen.


@;---------

@subsubsection{color-mode}

@bold{Name: } @defidentifier[#'color-mode]

Sets the color mode to rgb or hsb.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
   (no-stroke)
   (color-mode 'rgb 100)
   (eval:alts        (for ([i 100])
                       (for ([j 100])
                         (stroke i j 0)
                         (point i j)))
              (begin (for ([i 100])
                       (for ([j 100])
                         (stroke i j 0)
                         (point i j)))
                     (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
   (no-stroke)
   (color-mode 'hsb 100)
   (eval:alts        (for ([i 100])
                       (for ([j 100])
                         (stroke i j 0)
                         (point i j)))
              (begin (for ([i 100])
                       (for ([j 100])
                         (stroke i j 100)
                         (point i j)))
                     (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(color-mode mode)]                         @linebreak[]
@racketusage[(color-mode mode max)]                     @linebreak[]
@racketusage[(color-mode mode max1 max2 max3)]          @linebreak[]
@racketusage[(color-mode mode max1 max2 max3 maxA)]     @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[mode]  "'rgb or 'hsb corresponding to red/greeb/blue or huse/saturation/brightness")
               (list @racketid[max]   "maximum value for all color elements")
               (list @racketid[max1]  "maximum value for the red value or hue value according to the mode")
               (list @racketid[max2]  "maximum value for the green value or saturation value according to the mode")
               (list @racketid[max3]  "maximum value for the blue value or brightness value according to the mode")
               (list @racketid[maxA]  "maximum value for the alpha value"))]


@bold{Description}

Changes the way Sketching interprets color data. By default, the
parameters for @racket[fill], @racket[stroke], @racket[background], and @racket[color] are defined
by values between 0 and 255 using the RGB color model. The @racket[color-mode]
function is used to change the numerical range used for specifying
colors and to switch color systems. For example, calling
@racket[(color-mode 'rgb 1.0)] will specify that values are specified between 0
and 1. The limits for defining colors are altered by setting the
parameters @racketid[max], @racketid[max1], @racketid[max2], @racketid[max3], and @racketid[maxA].

After changing the range of values for colors, in expressions like
@racket[(color-mode 'hsb 360 100 100)], those ranges remain in use until they
are explicitly changed again. For example, after running
@racket[(color-mode 'hsb 360 100 100)] and then changing back to
@racket[(color-mode 'rgb)], the range for R will be 0 to 360 and the range for G
and B will be 0 to 100. To avoid this, be explicit about the ranges
when changing the color mode. For instance, instead of @racket[(color-mode 'rgb)],
write @racket[(color-mode 'rgb 255 255 255)].


@;---------

@subsubsection{lerp-color}

@bold{Name: } @defidentifier[#'lerp-color]

Calculates a color between two colors at a specific increment. 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (stroke 255)
          (background 51)
          (define from (color 204 102   0))
          (define to   (color   0 102 153))
          (define interA (lerp-color from to .33))
          (define interB (lerp-color from to .66))
          (fill from)
          (rect 10 20  20 60)
          (fill interA)
          (rect 30 20  20 60)
          (fill interB)
          (rect 50 20  20 60)
          (fill to)
          (eval:alts        (rect 70 20  20 60)
                     (begin (rect 70 20  20 60)
                            (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(lerp-color color1 color22 amount)]                         @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[color1]  "interpolate from this color")
               (list @racketid[color2]  "interpolate to this color")
               (list @racketid[amount]  "value between 0.0 and 1.0"))]


@bold{Description}

Calculates a color between two colors at a specific increment. The amount
argument is the amount to interpolate between the two values where
0.0 is equal to the first point, 0.1 is very near the first point, 0.5
is halfway in between, etc.

An amount below 0 will be treated as 0. Likewise, amounts above 1 will
be capped at 1. This is different from the behavior of @racket[lerp], but
necessary because otherwise numbers outside the range will produce
strange and unexpected colors.

@;---------

@subsection{2D Primitives}



@;---------

@subsubsection{arc}

@bold{Name: } @defidentifier[#'arc]

Draws an arc to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]


@examples[#:label #f #:eval se
          (stroke 0)
          (fill 255)
          (arc 50 55 50 50 0 π/2)
          (no-fill)
          (arc 50 55 60 60 π/2    π)
          (arc 50 55 70 70 π      (+ π π/4))
          (arc 50 55 80 80 (+ π π/4) 2π)
          (eval:alts        (arc 50 55 80 80 (+ π π/4) 2π)
                     (begin (arc 50 55 80 80 (+ π π/4) 2π)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]


@examples[#:label #f #:eval se
          (eval:alts        (arc 50 50 80 80 0 (* 5/4 pi) 'open-pie)
                     (begin (arc 50 50 80 80 0 (* 5/4 pi) 'open-pie)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]


@examples[#:label #f #:eval se
          (eval:alts        (arc 50 50 80 80 0 (* 5/4 pi) 'pie)
                     (begin (arc 50 50 80 80 0 (* 5/4 pi) 'pie)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]


@examples[#:label #f #:eval se
          (eval:alts        (arc 50 50 80 80 0 (* 5/4 pi) 'open)
                     (begin (arc 50 50 80 80 0 (* 5/4 pi) 'open)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (fill 255) (color-mode 'rgb 255)]


@examples[#:label #f #:eval se
          (eval:alts        (arc 50 50 80 80 0 (* 5/4 pi) 'chord)
                     (begin (arc 50 50 80 80 0 (* 5/4 pi) 'chord)
                            (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(arc a b c d start stop)]                         @linebreak[]
@racketusage[(arc a b c d start stop mode)]                    @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[a]       "x-coordinate of the arc's ellipse")
               (list @racketid[b]       "y-coordinate of the arc's ellipse")
               (list @racketid[c]       "width of the arc's ellipse by default")
               (list @racketid[d]       "height of the arc's ellipse by default")
               (list @racketid[start]   "angle to start the arc, specified in radians")
               (list @racketid[stop]    "angle to stop the arc, specified in radians")
               (list @racketid[mode]    "one of: 'open-pie 'pie 'open 'chord"))]


@bold{Description}

Draws an arc to the screen. Arcs are drawn along the outer edge of an
ellipse defined by the a, b, c, and d parameters. The center of the
arc's ellipse may be changed with the @racket[ellipse-mode] function. Use the
start and stop parameters to specify the angles (in radians) at which
to draw the arc. The start/stop values must be in clockwise order.

There are three ways to draw an arc; the rendering technique used is
defined by the optional seventh parameter. The four options, depicted
in the above examples, are @racket['open-pie]. @racket['pie], @racket['open], and @racket['chord].
The default mode is @racket['open-pie] (open stroke with pie fill).


@;---------

@subsubsection{circle}

@bold{Name: } @defidentifier[#'circle]

Draws a circle to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (circle 56 46 55)
                     (begin (circle 56 46 55)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(circle x y extent)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]       "x-coordinate of the arc's ellipse")
               (list @racketid[y]       "y-coordinate of the arc's ellipse")
               (list @racketid[extent]  "width of the arc's ellipse by default"))]


@bold{Description}

Draws a circle to the screen. By default, the first two parameters set
the location of the center, and the third sets the shape's width and
height. The center may be changed with the @racket[ellipse-mode] function.


@;---------

@subsubsection{ellipse}

@bold{Name: } @defidentifier[#'ellipse]

Draws an ellipse to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (ellipse 50 50 75 55)
                     (begin (ellipse 50 50 75 55)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(ellipse a b c d)]            @linebreak[]

@bold{Arguments}

The default interpretation of the arguments is:

@tabular[#:sep @hspace[1]
         (list (list @racketid[a]   "x-coordinate of the arc's ellipse")
               (list @racketid[b]   "y-coordinate of the arc's ellipse")
               (list @racketid[c]   "width of the arc's ellipse by default")
               (list @racketid[d]   "height of the arc's ellipse by default"))]

The interpretation of the arguments is affected by @racket[ellipse-mode].


@bold{Description}

Draws an ellipse (oval) to the screen. An ellipse with equal width and
height is a circle. By default, the first two parameters set the
location, and the third and fourth parameters set the shape's width
and height. The center may be changed with the @racket[ellipse-mode] function.

@;---------

@subsubsection{line}

@bold{Name: } @defidentifier[#'line]

Draws a line to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (line 30 20 85 75)
                     (begin (line 30 20 85 75)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (line 30 20 85 20)
          (stroke 126)
          (line 85 20 85 75)
          (stroke 255)
          (eval:alts        (line 85 75 30 75)
                     (begin (line 85 75 30 75)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(line x1 y1 x2 y2)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]   "x-coordinate of the first point")
               (list @racketid[y1]   "y-coordinate of the first point")
               (list @racketid[x2]   "x-coordinate of the second point")
               (list @racketid[y2]   "y-coordinate of the second point"))]



@bold{Description}

Draws a line (a direct path between two points) to the screen.  To color
a line, use the @racket[stroke] function. A line cannot be filled, therefore
the @racket[fill] function will not affect the color of a line. Lines are
drawn with a width of one pixel by default, but this can be changed
with the @racket[stroke-weight] function. 


@;---------

@subsubsection{point}

@bold{Name: } @defidentifier[#'point]

Draws a point to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
                            (no-smooth)
                            (point 30 20)
                            (point 85 20)
                            (point 85 75)
          (eval:alts        (point 30 75)
                     (begin (point 30 75)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
                            (no-smooth)
                            (stroke-weight 10)
          
                            (stroke-cap 'round)
                            (point 20 50)
                            (stroke-cap 'projecting)
                            (point 50 50)
                            (stroke-cap 'butt)                            
          (eval:alts        (point 80 50)
                     (begin (point 80 50)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
                            (stroke-weight 10)
          
                            (stroke-cap 'round)
                            (point 20 50)
                            (stroke-cap 'projecting)
                            (point 50 50)
                            (stroke-cap 'butt)                            
          (eval:alts        (point 80 50)
                     (begin (point 80 50)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(point x y)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "x-coordinate of the point")
               (list @racketid[y]   "y-coordinate of the point"))]



@bold{Description}

Draws a point, a coordinate in space at the dimension of one
pixel. The first parameter is the horizontal value for the point, the
second value is the vertical value for the point.

Use @racket[stroke] to set the color of the point.

Point appears round with the default @racket[(stroke-cap 'round)] and square with
@racket[(stroke-cap 'projecting)]. Points are invisible with @racket[stroke-cap 'square] 
(no cap).


@;---------

@subsubsection{quad}

@bold{Name: } @defidentifier[#'quad]

Draws a quadrilateral (a four-sided polygon) to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (quad 38 31 86 20 69 63 30 76)
                     (begin (quad 38 31 86 20 69 63 30 76)
                            (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(quad x1 y1  x2 y2  x3 y3  x4 y4)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]   "x-coordinate of the first corner")
               (list @racketid[y1]   "y-coordinate of the first corner")
               (list @racketid[x2]   "x-coordinate of the second corner")
               (list @racketid[y2]   "y-coordinate of the second corner")
               (list @racketid[x3]   "x-coordinate of the third corner")
               (list @racketid[y3]   "y-coordinate of the third corner")
               (list @racketid[x4]   "x-coordinate of the fourth corner")
               (list @racketid[y4]   "y-coordinate of the fourth corner"))]


@bold{Description}

Draws a quadrilateral (a four-sided polygon) to the screen.

A quad is a quadrilateral, a four sided polygon. It is similar to a
rectangle, but the angles between its edges are not constrained to
ninety degrees. List the points either in  clockwise or
counter-clockwise ordner around the defined shape.


@;---------

@subsubsection{rect}

@bold{Name: } @defidentifier[#'rect]

Draws a rectangle to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (rect 30 20 55 55)
                     (begin (rect 30 20 55 55)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (rect 30 20 55 55 7)
                     (begin (rect 30 20 55 55 7)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@void["examples[#:label #f #:eval se          
          (eval:alts        (rect 30 20 55 55 3 6 12 18)
                     (begin (rect 30 20 55 55 3 6 12 18)
                            (send dc get-bitmap)))]"]



@bold{Usage}

@racketusage[(rect a b c d)]              @linebreak[]
@racketusage[(rect a b c d r)]            @linebreak[]
@;@racket[(rect a b c d tl tr br bl)]  @linebreak[] TODO

@bold{Arguments}


The default interpretation of the arguments is:


@tabular[#:sep @hspace[1]
         (list (list @racketid[a]    "x-coordinate of corner")
               (list @racketid[b]    "y-coordinate of corner")
               (list @racketid[c]    "width")
               (list @racketid[d]    "height")
               (list @racketid[r]    "radii for all four corners")
               #;(list @racketid[tl]   "radius for top-left corner")
               #;(list @racketid[tr]   "radius for top-right corner")
               #;(list @racketid[br]   "radius for bottom-right corner")
               #;(list @racketid[bl]   "radius for bottom-left corner"))]

The interpretation of the arguments is affected by @racket[rect-mode].


@bold{Description}

Draws a rectangle to the screen. A rectangle is a four-sided shape
with every angle at ninety degrees. By default, the first two
arguments set the location of the upper-left corner, the third sets
the width, and the fourth sets the height. The way these arguments
are interpreted, however, may be changed with the @racket[rect-mode] function.

To draw a rounded rectangle, add a fifth argument, which is used as
the radius value for all four corners.

To use a different radius value for each corner, use eight
argument. When using eight parameters, the latter four set the
radius of the arc at each corner separately, starting with the
top-left corner and moving clockwise around the rectangle.


@;---------

@subsubsection{square}

@bold{Name: } @defidentifier[#'square]

Draws a square to the screen.

@bold{Example}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (square 30 20 55)
                     (begin (square 30 20 55)
                            (send dc get-bitmap)))]




@bold{Usage}

@racketusage[(square x y extent)]              @linebreak[]

@bold{Arguments}


The default interpretation of the arguments is:


@tabular[#:sep @hspace[1]
         (list (list @racketid[x]      "x-coordinate of corner")
               (list @racketid[y]      "y-coordinate of corner")
               (list @racketid[extent] "length of side"))]

The interpretation of the arguments is affected by @racket[rect-mode].


@bold{Description}

Draws a square to the screen. A square is a four-sided shape with
every angle at ninety degrees and each side is the same length. By
default, the first two arguments set the location of the upper-left
corner, the third sets the width and height. The way these arguments
are interpreted, however, may be changed with the @racket[rect-mode] function.

@;---------

@subsubsection{triangle}

@bold{Name: } @defidentifier[#'triangle]

Draws a triangle to the screen.

@bold{Example}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (eval:alts        (triangle 30 75 58 20 86 75)
                     (begin (triangle 30 75 58 20 86 75)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(triangle x1 y1  x2 y2  x3 y3)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]   "x-coordinate of the first corner")
               (list @racketid[y1]   "y-coordinate of the first corner")
               (list @racketid[x2]   "x-coordinate of the second corner")
               (list @racketid[y2]   "y-coordinate of the second corner")
               (list @racketid[x3]   "x-coordinate of the third corner")
               (list @racketid[y3]   "y-coordinate of the third corner"))]

@bold{Description}

A triangle is a shape created by connecting three points. The first
two arguments specify the first point, the middle two arguments
specify the second point, and the last two arguments specify the third
point.

@;---------
@;---------
@;---------

@subsection{Curves}

@;---------

@subsubsection{bezier}

@bold{Name: } @defidentifier[#'bezier]

Draws a Bezier curve to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (no-fill)
          (stroke 255 102 0)
          (line 85 20 10 10)
          (line 90 90 15 80)
          (stroke 0 0 0)                    
          (eval:alts        (bezier 85 20 10 10 90 90 15 80)
                     (begin (bezier 85 20 10 10 90 90 15 80)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (no-fill)
          (stroke 255 102 0)
          (line 30 20 80 5)
          (line 80 75 30 75)
          (stroke 0 0 0)                    
          (eval:alts        (bezier 30 20  80 5  80 75  30 75)
                     (begin (bezier 30 20  80 5  80 75  30 75)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(bezier x1 y1 x2 y2 x3 y3 x4 y4)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]   "x-coordinate of the first anchor point")
               (list @racketid[y1]   "y-coordinate of the first anchor point")
               (list @racketid[x2]   "x-coordinate of the second control point")
               (list @racketid[y2]   "y-coordinate of the second control point")
               (list @racketid[x3]   "x-coordinate of the third control point")
               (list @racketid[y3]   "y-coordinate of the third control point")
               (list @racketid[x4]   "x-coordinate of the fourth anchor point")
               (list @racketid[y4]   "y-coordinate of the fourth anchor point"))]

@bold{Description}

Draws a Bezier curve on the screen. These curves are defined by a
series of anchor and control points. The first two parameters specify
the first anchor point and the last two parameters specify the other
anchor point. The middle parameters specify the control points which
define the shape of the curve. Bezier curves were developed by French
engineer Pierre Bezier. 

@;---------
@;---------
@;---------


@subsection{Attributes}

@;---------

@subsubsection{ellipse-mode}

@bold{Name: } @defidentifier[#'ellipse-mode]

Modifies the location from which ellipses are drawn.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (ellipse-mode 'radius)
          (fill "white")
          (ellipse 50 50 30 30)

          (ellipse-mode 'center)
          (fill "darkgray")          
          (eval:alts        (ellipse 50 50 30 30)
                     (begin (ellipse 50 50 30 30)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (ellipse-mode 'corner)
          (fill "white")
          (ellipse 25 25 50 50)

          (ellipse-mode 'corners)
          (fill "darkgray")          
          (eval:alts        (ellipse 25 25 50 50)
                     (begin (ellipse 25 25 50 50)
                            (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(ellipse-mode mode)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[mode] "one of: 'center 'radius 'corner 'corners"))]

@bold{Description}

Modifies the location from which ellipses are drawn by changing the
way in which arguments given to @racket[ellipse] are intepreted.

The default mode is @racket[(ellipse-mode 'center)], which interprets the first
two parameters of @racket[ellipse] as the shape's center point, while the
third and fourth arguments are its width and height.

@racket[(ellipse-mode 'radius)] also uses the first two arguments of @racket[ellipse] as
the shape's center point, but uses the third and fourth arguments to
specify half of the shapes's width and height.

@racket[(ellipse-mode 'corner)] interprets the first two arguments of @racket[ellipse]
as the upper-left corner of the shape, while the third and fourth
arguments are its width and height.

@racket[(ellipse-mode 'corners)] interprets the first two arguments of @racket[ellipse]
as the location of one corner of the ellipse's bounding box, and the
third and fourth arguments as the location of the opposite corner.


@;---------

@subsubsection{rect-mode}

@bold{Name: } @defidentifier[#'rect-mode]

Modifies the location from which rectangles and squares are drawn.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (rect-mode 'corner)
          (fill "white")
          (rect 25 25 50 50)

          (rect-mode 'corners)
          (fill "darkgray")          
          (eval:alts        (rect 25 25 50 50)
                     (begin (rect 25 25 50 50)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (rect-mode 'radius)
          (fill "white")
          (rect 50 50 30 30)

          (rect-mode 'center)
          (fill "darkgray")          
          (eval:alts        (rect 50 50 30 30)
                     (begin (rect 50 50 30 30)
                            (send dc get-bitmap)))]


@examples[#:hidden #:eval se (rect-mode 'corner)]

@bold{Usage}

@racketusage[(rect-mode mode)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[mode] "one of: 'center 'radius 'corner 'corners"))]

@bold{Description}

Modifies the location from which rectangles and squares are drawn by changing the
way in which arguments given to @racket[rect] and @racket[square] are intepreted.

The default mode is @racket[(rect-mode 'corner)], which interprets the first two
arguments of @racket[rect] as the upper-left corner of the shape, while the
third and fourth arguments are its width and height.

@racket[(rect-mode 'corners)] interprets the first two arguments of @racket[rect] as the
location of one corner, and the third and fourth arguments as the
location of the opposite corner.

@racket[(rect-mode 'center)] interprets the first two arguments of @racket[rect] as the
shape's center point, while the third and fourth arguments are its
width and height.

@racket[(rect-mode 'radius)] also uses the first two arguments of @racket[rect] as the
shape's center point, but uses the third and fourth arguments to
specify half of the shapes's width and height.

@;---------

@subsubsection{stroke-cap}

@bold{Name: } @defidentifier[#'stroke-cap]

Sets the style for rendering line endings. 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (stroke-weight 12)
          (stroke-cap 'round)
          (line 20 30 80 30)
          (stroke-cap 'square)
          (line 20 50 80 50)
          (stroke-cap 'project)
          (eval:alts        (line 20 70 80 70)
                     (begin (line 20 70 80 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(stroke-cap cap)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[cap] "one of: 'round 'square 'project"))]

@bold{Description}

Sets the style for rendering line endings. These ends are either
squared, extended, or rounded, each of which specified with the
corresponding parameters: @racket['square], @racket['project], and @racket['round].
The default cap is @racket['round].

To make @racket[point] appear square, use @racket[(stroke-cap 'project)].
Using @racket[(stroke-cap 'square)] (no cap) causes points to become invisible.

@;---------

@subsubsection{stroke-join}

@bold{Name: } @defidentifier[#'stroke-join]

Sets the style of the joints which connect line segments. 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (no-fill)
          (stroke-weight 12)
          (stroke-join 'miter)
          (begin-shape)
            (vertex 35 20)
            (vertex 65 50)
            (vertex 35 80)          
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (no-fill)
          (stroke-weight 12)
          (stroke-join 'bevel)
          (begin-shape)
            (vertex 35 20)
            (vertex 65 50)
            (vertex 35 80)          
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se          
          (no-fill)
          (stroke-weight 12)
          (stroke-join 'round)
          (begin-shape)
            (vertex 35 20)
            (vertex 65 50)
            (vertex 35 80)          
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(stroke-join join)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[join] "one of: 'miter 'bevel 'round"))]

@bold{Description}

Sets the style of the joints which connect line segments. These joints
are either mitered, beveled, or rounded and specified with the
dcorresponding parameters @racket['miter], @racket['bevel] and @racket['round].
The default joint is @racket['miter].

@;---------

@subsubsection{stroke-weight}

@bold{Name: } @defidentifier[#'stroke-weight]

Sets the width of the stroke used for lines, points, and the border around shapes.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (stroke-weight 1)
          (line 20 20 80 20)
          (stroke-weight 4)
          (line 20 40 80 40)
          (stroke-weight 10)
          (line 20 70 80 70)
          (eval:alts        (line 20 70 80 70)
                     (begin (line 20 70 80 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(stroke-weight weight)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[weight] "the weight (in pixels) of the stroke"))]

@bold{Description}

Sets the width of the stroke used for lines, points, and the border
around shapes. All widths are set in units of pixels.

The default stroke weight is 1.

@;---------
@;---------
@;---------


@subsection{Shapes}


@;---------

@subsubsection{begin-shape}

@bold{Name: } @defidentifier[#'begin-shape]

Starts recording points used to draw a complex shape.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape 'close)
                     (begin (end-shape 'close)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (no-fill)
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape 'points)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape 'lines)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape 'triangles)
            (vertex 30 75)
            (vertex 40 20)
            (vertex 50 75)
            (vertex 60 20)
            (vertex 70 75)
            (vertex 80 20)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape 'triangle-strip)
            (vertex 30 75)
            (vertex 40 20)
            (vertex 50 75)
            (vertex 60 20)
            (vertex 70 75)
            (vertex 80 20)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape 'triangle-fan)
            (vertex 50 50)
            (vertex 50 15)
            (vertex 85 50)
            (vertex 50 85)
            (vertex 15 50)
            (vertex 50 15)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape 'quads)
          (vertex 30 20)
          (vertex 30 75)
          (vertex 50 75)
          (vertex 50 20)
          
          (vertex 65 20)
          (vertex 65 75)
          (vertex 85 75)
          (vertex 85 20)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape 'quad-strip)
          (vertex 30 20)
          (vertex 30 75)
          (vertex 50 75)
          (vertex 50 20)
          (vertex 65 20)
          (vertex 65 75)
          (vertex 85 75)
          (vertex 85 20)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (begin-shape)
          (vertex 20 20)
          (vertex 40 20)
          (vertex 40 40)
          (vertex 60 40)
          (vertex 60 60)
          (vertex 20 60)
          (eval:alts        (end-shape 'close)
                     (begin (end-shape 'close)
                            (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(begin-shape)]        @linebreak[]

@;bold{Arguments}
@;tabular[#:sep @hspace[1] (list (list @racketid[weight] "the weight (in pixels) of the stroke"))]

@bold{Description}

Starts recording points used to draw a complex shape. Use the @racket[vertex] functions
to add points to the shape. When all points are added, use @racket[end-shape] to draw
the shape.

@;---------

@subsubsection{end-shape}

@bold{Name: } @defidentifier[#'end-shape]

Draws the current shape to the screen - and stops the recording of points.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape 'close)
                     (begin (end-shape 'close)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (no-fill)
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(end-shape)]          @linebreak[]
@racketusage[(end-shape mode)]     @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[mode] @racket['close]))]


@bold{Description}

Draws the current shape to the screen - and stops the recording of points.
The function @racket[end-shape] may only be called after @racket[begin-shape].
When @racket[end-shape] is called, all data recorded since the last call to @racket[begin-shape]
is drawn to the screen.

If the mode @racket['close] is used, the shape will be close (the end and start point will
be connected).

@;---------

@subsubsection{vertex}

@bold{Name: } @defidentifier[#'vertex]

Add a point to the current shape.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape 'close)
                     (begin (end-shape 'close)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (no-fill)
          (begin-shape)
            (vertex 30 20)
            (vertex 85 20)
            (vertex 85 75)
            (vertex 30 75)
          (eval:alts        (end-shape)
                     (begin (end-shape)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(vertex x y)]          @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[x] "x-coordinate of the point")
               (list @racketid[y] "y-coordinate of the point"))]


@bold{Description}

Add the point (x,y) to the current shape.
The @racket[vertex] functions must be called between @racket[begin-shape]
and @racket[end-shape].

@;---------
@;---------
@;---------

@subsection{Typography}

@;---------

@subsubsection{text}

@bold{Name: } @defidentifier[#'text]

Draws text to the screen.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (text-size 32)
          (fill 255)
          (text "word" 10 30)
          (fill 0 102 153)
          (text "word" 10 60)
          (fill 0 102 153 51)
          (eval:alts        (text "word" 10 90)
                     (begin (text "word" 10 90)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (text-size 11)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (code:comment "Pass width and height in order to draw multi line text.")
          (text-size 11)
          (fill 0)
          (define s "The quick brown fox jumps over the lazy dog.")
          (eval:alts        (text s 10 10 70 80)
                     (begin (text s 10 10 70 80)
                            (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(text s x y)]          @linebreak[]
@racketusage[(text s x1 y1 x2 y2)]  @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[s]  "a string")
               (list @racketid[x]  "x-coordinate")
               (list @racketid[y]  "y-coordinate")
               (list @racketid[x1] "x-coordinate")
               (list @racketid[y1] "y-coordinate")
               (list @racketid[x2] "number")
               (list @racketid[y2] "number"))]

The interpretation of the arguments is affected by @racket[rect-mode].


@bold{Description}

Draws text to the screen.

If @racket[text] is used as @racketusage[(text s x y)] then
the point @racketid[(x,y)] determines the position of the text.
How the text is placed relative to this point is determined by
the alignment settings made by @racket[text-align].

If @racket[text] is used as @racketusage[(text s x1 y1 x2 y2)] then
the point @racketid[(x1,y1)] and the numbers @racketusage[x2] and
@racketusage[y2] are interpreted according to the mode
set by @racket[rect-mode]. In the default mode, @racketusage[x2]
and @racketusage[y2] are width and height.

Only the text that fits completely inside the rectangular area
is drawn to screen.

Use @racket[fill] to change the color of the text.


@;---------

@subsubsection{text-align}

@bold{Name: } @defidentifier[#'text-align]

Sets the horizontal and vertical alignment for drawing text. 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 16)
          (text-align 'right)
          (text "ABCD" 50 30)
          (text-align 'center)
          (text "EFGH" 50 50)
          (text-align 'left)
          (eval:alts        (text "IJKL" 50 70)
                     (begin (text "IJKL" 50 70)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (text-size 11)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (background 0)
          (stroke 153)
          (fill 255)
          (text-size 11)
          
          (text-align 'center 'bottom)
          (line 0 20 width 20)
          (text "center, bottom" 50 20)

          (text-align 'center 'center)
          (line 0 40 width 40)
          (text "center, center" 50 40)

          (text-align 'center 'top)
          (line 0 60 width 60)
          (text "center, top" 50 60)

          (text-align 'center 'baseline)
          (line 0 90 width 90)                   
          (eval:alts        (text "center, baseline" 50 90)
                     (begin (text "center, baseline" 50 90)
                            (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(text-align x-alignment)]              @linebreak[]
@racketusage[(text-align x-alignment y-alignment)]  @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[x-alignment]  "one of 'left 'center 'right")
               (list @racketid[y-alignment]  "one of 'top  'center 'bottom 'baseline"))]


@bold{Description}

Sets the horizontal and vertical alignment for drawing text.

The @racketid[x-alignment] can be one of the symbols @racket['left],
@racket['center] and @racket['right]. The setting affects how
@racket[text] draws the text relative to the position given to
@racket[text]. 

The optional second argument @racketid[y-alignment] is used to vertically
align the text. If the second argument isn't used, then the
vertical alignment will be set to the default value @racket['baseline].

The settings @racket['top], @racket['center], @racket['bottom] and
@racket['baseline] will put the position given to @racket[text]
such that the position is at the top of, center of, bottom of, or,
at the baseline of the text respectively. This applies when @racket[text]
is used to draw a single text line with @racketusage[(text s x y)].

For multiple line text drawn with @racketusage[(text s x1 y1 x2 y2)]
the alignment applies to the individual lines inside the rectangular
area given by the arguments. When used for multiple lines of text
the vertical alignment @racket['baseline] is not available.

The vertical alignment is based on the value on the ascent specified
in the font. Some fonts do not specify this correctly, so sometime
you will need to adjust the y-coordinate with a few pixels manually.


@;---------

@subsubsection{text-size}

@bold{Name: } @defidentifier[#'text-size]

Sets the font size. 

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 26)
          (text "WORD" 10 50)
          (text-size 14)                    
          (eval:alts        (text "WORD" 10 70)
                     (begin (text "WORD" 10 70)
                            (send dc get-bitmap)))]




@bold{Usage}

@racketusage[(text-size size)]              @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[size]  "the size (measured in pixels as default)"))]


@bold{Description}

Sets the font size. 


@;---------

@subsubsection{text-face}

@bold{Name: } @defidentifier[#'text-face]

Sets the font face. Examples of font faces: "Courier", "Arial".

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 30)
          (text-align 'center 'center)
          (text-face "Times")                          
          (eval:alts        (text "Word" 50 50)
                     (begin (text "Word" 50 50)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-face face)]              @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[face]  "string: a font face"))]


@bold{Description}

Sets the font face. Some well-known font faces are "Courier" and "Arial".
The format and meaning of the font faces are platform- and device-specific.

@;---------

@subsubsection{text-family}

@bold{Name: } @defidentifier[#'text-family]

Sets the font family.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 30)
          (text-align 'center 'center)
          (text-face #f)
          (text-family 'roman)
          (text "word" 50 20)
          (text-family 'decorative)
          (text "word" 50 50)
          (text-family 'modern)
          (eval:alts        (text "word" 50 80)
                     (begin (text "word" 50 80)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-family family)]              @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[family]  "string: a font family"))]


@bold{Description}

Sets the font family. The available families are:
@racket['default], @racket['decorative],  @racket['roman], @racket['script],
@racket['swiss], @racket['modern],  @racket['symbol], @racket['system].

If the font face is @racket[#f] then the font appearance is determined
solely by the font family.

@;---------

@subsubsection{text-weight}

@bold{Name: } @defidentifier[#'text-weight]

Sets the font weight.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline) (text-weight 'thin)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 30)
          (text-align 'center 'center)
          (text-weight 'normal)
          (text "word" 50 30)
          (text-weight 'bold)
          (eval:alts        (text "word" 50 70)
                     (begin (text "word" 50 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-weight weight)]              @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[weight]  "a number between 100 and 1000, or")
               (list ""                 "one of the weight symbols"))]


@bold{Description}

Sets the font weight. The available font weights are listed
in the document for @racket[font%].

@;---------

@subsubsection{text-underlined?}

@bold{Name: } @defidentifier[#'text-underlined?]

Sets whether the text is underlined or not.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline) (text-weight 'thin)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 30)
          (text-align 'center 'center)
          (text-underlined? #t)
          (text "word" 50 30)
          (text-underlined? #f)
          (eval:alts        (text "word" 50 70)
                     (begin (text "word" 50 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-underlined? underlined?)]              @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[underlined?]  "boolean: #t or #f"))]


@bold{Description}

Sets whether the text is underlined or not.

@;---------

@subsubsection{text-smoothing}

@bold{Name: } @defidentifier[#'text-smoothing]

Sets the amount of smoothing.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline) (text-weight 'thin)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)          
          (text-size 30)
          (text-align 'center 'center)
          (text-smoothing 'unsmoothed)
          (text "word" 50 30)
          (text-smoothing 'smoothed)
          (eval:alts        (text "word" 50 70)
                     (begin (text "word" 50 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-smoothing smoothing)]              @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[smoothing]  "one of: 'default 'partly-smoothed 'smoothed 'unsmoothed"))]


@bold{Description}

Sets the amount of smoothing.


@;---------

@subsubsection{text-size-in-pixels?}

@bold{Name: } @defidentifier[#'text-size-in-pixels?]

Sets whether the font size is specified in pixels or in logical
drawing units. For non-high-resolution screens this is the same.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline) (text-weight 'thin)]


@examples[#:label #f #:eval se
          (background 0)
          (fill 255)
          (text-size-in-pixels? #t)
          (text-size 30)
          (text-align 'center 'center)
          (text "word" 50 30)
          (text-size-in-pixels? #f)
          (text-size 30)
          (eval:alts        (text "word" 50 70)
                     (begin (text "word" 50 70)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(text-size-in-pixels? pixels?)]              @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[pixels?]  "boolean: one of #f or #f"))]


@bold{Description}

Sets whether @racket[text-size] uses pixels or logical drawing
units.


@;---------

@subsubsection{text-hinting}

@bold{Name: } @defidentifier[#'text-hinting]

Sets whether the font metrics should be rounded to integers.



@bold{Usage}

@racketusage[(text-hinting hint)]              @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[hint]  "one of: 'aligned 'unaligned"))]


@bold{Description}

Sets whether the font metrics should be rounded to integers.
The default is @racket['unaligned] which improves the consistency
of letter spacing for pixel-based targets, but at the expense
of making metrics unscalable.


@;---------
@;---------
@;---------

@subsection{Image}

@;---------

@subsubsection{image}

@bold{Name: } @defidentifier[#'image]

Draws an image on the canvas.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (define img (load-image "laDefense.jpg"))
          (eval:alts        (image img 0 0)
                     (begin (image img 0 0)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (text-size 11)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@; TODO implement the five argument version of image first...
@;examples[#:label #f #:eval se
@;          (define img (load-image "laDefense.jpg"))
@;          (image img 0 0)
@;          (eval:alts        (image img 0 0 (/ width 2) (/ height 2))
@;                     (begin (image img 0 0 (/ width 2) (/ height 2))
@;                            (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(image img x y)]          @linebreak[]
@;(racketusage[(image img x1 y1 x2 y2)]  @linebreak[])


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[img] "a bitmap image")
               (list @racketid[x]  "x-coordinate")
               (list @racketid[y]  "y-coordinate")
               #;(list @racketid[x1] "x-coordinate")
               #;(list @racketid[y1] "y-coordinate")
               #;(list @racketid[x2] "number")
               #;(list @racketid[y2] "number"))]

The interpretation of the arguments is affected by @racket[image-mode].


@bold{Description}

Draws an image on the canvas.

The function @racket[image] draws an image on the canvas.
You can use image formats such as gif, jpeg and png.

The @racketid[img] argument specifies the image to display
and by default the @racketid[x1] and @racketid[y1]
arguments define the location of its upper-left corner.
The function @racket[image-mode] can be used to
change the way these arguments are interpreted.

The color of an image may be modified with the function @racket[tint].
This function will maintain transparency for gif and png images.


@;---------

@subsubsection{image-width}

@bold{Name: } @defidentifier[#'image-width]

Returns the width of an image.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (define img (load-image "moonwalk.jpg"))
          (eval:alts        (image img 0 0)
                     (begin (image img 0 0)
                            (send dc get-bitmap)))
          (image-width img)]


@bold{Usage}

@racketusage[(image-width img)]          @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[img] "a bitmap image"))]


@bold{Description}

Returns the width of an image (bitmap).

@;---------

@subsubsection{image-height}

@bold{Name: } @defidentifier[#'image-height]

Returns the height of an image.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)
          (rect-mode 'corner) (text-align 'left 'baseline)]


@examples[#:label #f #:eval se
          (define img (load-image "moonwalk.jpg"))
          (eval:alts        (image img 0 0)
                     (begin (image img 0 0)
                            (send dc get-bitmap)))
          (image-height img)]


@bold{Usage}

@racketusage[(image-height img)]          @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[img] "a bitmap image"))]


@bold{Description}

Returns the height of an image (bitmap).


@;---------

@subsubsection{image-mode}

@bold{Name: } @defidentifier[#'image-mode]

Modifies the location from which images (bitmaps) are drawn.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@;examples[#:label #f #:eval se
@;          (define img (load-image "laDefense.jpg"))
@;          (image-mode 'corner)
@;          (eval:alts        (image img 10 10 50 50)
@;                     (begin (image img 10 10 50 50)
@;                            (send dc get-bitmap)))]
@;
@examples[#:hidden #:eval se (image-mode 'corner)]

@bold{Usage}

@racketusage[(image-mode mode)]        @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[mode] "one of: 'center 'corner 'corners"))]

@bold{Description}

Modifies the location from which images (bitmaps) are drawn by changing the
way in which arguments given to @racket[image] are intepreted.

The default mode is @racket[(image-mode 'corner)], which interprets the first two
arguments of @racket[image] as the upper-left corner of the shape, while the
third and fourth arguments are its width and height.

@racket[(image-mode 'corners)] interprets the first two arguments of @racket[image] as the
location of one corner, and the third and fourth arguments as the
location of the opposite corner.

@racket[(image-mode 'center)] interprets the first two arguments of @racket[image] as the
shape's center point, while the third and fourth arguments are its
width and height.


@;---------
@;---------
@;---------

@subsection{Data}


@;---------

@subsubsection{color}

@bold{Name: } @defidentifier[#'color]

Datatype for storing color values.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define c1 (color 204 153 0))
          (define c2 "#ffcc00")
          (no-stroke)
          (fill c1)
          (rect 0 0 25 100)
          (fill c2)
          (rect 25 0 25 100)
          ;(define c3 (get 10 50))
          ;(fill c3)
          (eval:alts        (rect 50 0 50 100)
                     (begin (rect 50 0 50 100)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(color r g b)]          @linebreak[]
@racketusage[(color r g b a)]        @linebreak[]
@racketusage[(color h s b)]          @linebreak[]
@racketusage[(color h s b a)]        @linebreak[]
@racketusage["#rrggbb"]              @linebreak[]
@racketusage["#rrggbbaa"]            @linebreak[]
@racketusage[colorname]              @linebreak[]


@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racketid[r]         "red        component (default range: 0-255)")
               (list @racketid[g]         "green      component (default range: 0-255)")
               (list @racketid[b]         "blue       component (default range: 0-255)")
               (list @racketid[a]         "alpha      component (default range: 0-255)")
               (list @racketid[h]         "hue        component (default range: 0-255)")
               (list @racketid[s]         "saturation component (default range: 0-255)")
               (list @racketid[v]         "brightness component (default range: 0-255)")
               (list @racketid["#rrggbb"] "hexadecimal string")
               (list @racketid[colorname] "colorname as a string or symbol"))]


@bold{Description}

Datatype for storing color values. Colors may be assigned with @racket[get]
and @racket[color] or they may be specified directly using hexadecimal
notation such as @racket["#ffcc00"] or @racket["#ffffcc00"].


@;---------
@;---------
@;---------

@subsection{Time and Date}

@;---------

@subsubsection{day}

@bold{Name: } @defidentifier[#'day]

Returns current day as a number from 1 to 31.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (day)]


@bold{Usage}

@racketusage[(day)]          @linebreak[]

@bold{Description}

Returns the current day as a number from 1 to 31.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.

@;---------

@subsubsection{hour}

@bold{Name: } @defidentifier[#'hour]

Returns the current hour as a number from 0 to 23.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (hour)]


@bold{Usage}

@racketusage[(hour)]          @linebreak[]

@bold{Description}

Returns the current hour as a number from 0 to 23.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.

@;---------

@subsubsection{millis}

@bold{Name: } @defidentifier[#'millis]

Returns the number of milliseconds since starting the program.

@bold{Examples}

@examples[#:hidden #:eval se
          ; this is normally done by start
          (reset-milliseconds-at-start-of-program! (- (current-milliseconds) 20))]

@examples[#:label #f #:eval se
          (millis)]


@bold{Usage}

@racketusage[(millis)]          @linebreak[]

@bold{Description}

Returns the number of milliseconds since starting the program.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.

@;---------

@subsubsection{minute}

@bold{Name: } @defidentifier[#'minute]

Returns the current minutes as a number from 0 to 59.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (minute)]


@bold{Usage}

@racketusage[(minute)]          @linebreak[]

@bold{Description}

Returns the current minutes as a number from 0 to 59.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.


@;---------

@subsubsection{month}

@bold{Name: } @defidentifier[#'month]

Returns the current month as a number from 1 to 12.

@bold{Examples}

@examples[#:label #f #:eval se
          (month)]


@bold{Usage}

@racketusage[(month)]          @linebreak[]

@bold{Description}

Returns the current month as a number from 1 to 12.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.


@;---------

@subsubsection{second}

@bold{Name: } @defidentifier[#'second]

Returns the current second as a number from 0 to 59.

@bold{Examples}

@examples[#:label #f #:eval se
          (second)]


@bold{Usage}

@racketusage[(second)]          @linebreak[]

@bold{Description}

Returns the current second as a number from 0 to 59.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.

@;---------

@subsubsection{year}

@bold{Name: } @defidentifier[#'year]

Returns the current year as a number.

@bold{Examples}

@examples[#:label #f #:eval se
          (year)]


@bold{Usage}

@racketusage[(year)]          @linebreak[]

@bold{Description}

Returns the current year as a number.

If you need the corresponding string, use @racket[~a] or @racket[number->string]
to convert the number to a string.


@;---------
@;---------
@;---------

@subsection{Transform}

@;---------

@subsubsection{rotate}

@bold{Name: } @defidentifier[#'rotate]

Rotates the coordinate system around the origin.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (code:comment "place the origin in the center of the screen")
          (translate (/ width 2) (/ height 2))
          (code:comment "rotate π/3 radians (60 degrees)")
          (rotate (/ π 3))                  
          (eval:alts        (rect -26 -26 52 52)
                     (begin (rect -26 -26 52 52)
                            (send dc get-bitmap)))]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se          
          (code:comment "place the origin in the center of the screen")
          (translate (/ width 2) (/ height 2))
          (code:comment "flip the y-axis (now standard orientation)")
          (scale 1 -1)                         
          (fill 255)
          (rect -26 -26 52 52)
          (fill "red")
          (code:comment "rotate 10 degrees")
          (rotate (radians 10))                
          (eval:alts        (rect -26 -26 52 52)
                     (begin (rect -26 -26 52 52)
                            (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(rotate angle)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[angle] "the angle of rotation (in radians)"))]

@bold{Description}

Rotates the coordinate system around the origin. The argument @racketid[angle]
determines the amount to rotate. The angle is measured in radians (from 0 to 2π).
Use the function @racket[radians] to convert angles in degrees to radians.

The coordinate system is in the direction from the x-axis to the y-axis.
With the default screen coordinate system this looks like a clockwise
rotation. If the direction of the x-axis is flipped (so we have a standard
mathematical coordinate system), the rotation is in the standard counter clockwise
direction.

The coordinate system is rotated around the origin, so use @racket[translate]
to change the origin to the point of revolution before calling @racket[rotate].

Transformations apply to everything that happens afterward,
and subsequent calls to the function compound the effect. For example,
calling @racket[(rotate (/ pi 2))] once and then calling @racket[(rotate (/ pi 2))] a second
time is the same as a single @racket[(rotate pi)]. All tranformations are reset
when @racket[draw] begins again.

Technically, @racket[rotate] multiplies the current transformation matrix by
a rotation matrix. This function can be further controlled by
@racket[push-matrix] and @racket[pop-matrix].


@;---------

@subsubsection{scale}

@bold{Name: } @defidentifier[#'scale]

Changes the scales of the x- and y-axis.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]



@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (fill "white")
          (rect 30 20  50 50)
          (code:comment "use different scales on the axes")
          (scale 0.5 1.3) 
          (fill "red")
          (eval:alts        (rect 30 20  50 50)
                     (begin (rect 30 20  50 50)
                            (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(scale s)]        @linebreak[]
@racketusage[(scale sx sy)]      @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racketid[s]  "the scale factor for both x and y")
               (list @racketid[sx] "the scale factor for x")
               (list @racketid[sy] "the scale factor for y"))]

@bold{Description}

Changes the scales of the x- and y-axis. A scale factor greater than 1 will
increase the size of objects drawn and a scale factor between 0 and 1 will
decrease the size. The two argument call @racketusage[(scale sx sy)] can
be used to get different scale of the two axes.

Transformations apply to everything that happens afterward,
and subsequent calls to the function compound the effect. For example,
calling @racket[(scale 2)] once and then calling @racket[(scale 3)] a second
time is the same as a single @racket[(scale 6)]. All tranformations are reset
when @racket[draw] begins again.

Technically, @racket[scale] affects the current transformation matrix.
This function can be further controlled by @racket[push-matrix] and @racket[pop-matrix].


@;---------

@subsubsection{translate}

@bold{Name: } @defidentifier[#'translate]

Moves the origin.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (translate 30 20)          
          (eval:alts        (rect 0 0  55 55)
                     (begin (rect 0 0  55 55)
                            (send dc get-bitmap)))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
          (code:comment "draw rectagnel at orignal (0,0) i.e. top-left")
          (rect 0 0  55 55)
          (code:comment "move the origin to (30,20) then draw rectangle")
          (fill "red")
          (translate 30 20)
          (eval:alts        (rect 0 0  55 55)
                     (begin (rect 0 0  55 55)
                            (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(translate tx ty)]      @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racketid[tx] "the x-coordinate (in the current coordinate system) of the new origin")
               (list @racketid[ty] "the y-coordinate (in the current coordinate system) of the new origin"))]

@bold{Description}

Moves the origin to (@racketid[tx],@racketid[ty]).

Transformations apply to everything that happens afterward,
and subsequent calls to the function compound the effect. For example,
calling @racket[(translate 2 3)] once and then calling @racket[(translate 20 30)] a second
time is the same as a single @racket[(scale 22 33)]. All tranformations are reset
when @racket[draw] begins again.

Technically, @racket[translate] changes the current transformation matrix.
This function can be further controlled by @racket[push-matrix] and @racket[pop-matrix].

@;---------

@subsubsection{push-matrix}

@bold{Name: } @defidentifier[#'push-matrix]

Stores the current transformation matrix.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (code:comment "White rectangle")
          (fill 255)
          (rect 0 0 50 50)

          (code:comment "Black rectangle")
          (push-matrix)
          (translate 30 20)
          (fill 0)
          (rect 0 0 50 50)
          (pop-matrix)

          (code:comment "Gray rectangle")
          (fill 100)
          (eval:alts        (rect 15 10 50 50)
                     (begin (rect 15 10 50 50)
                            (send dc get-bitmap)))]



@bold{Description}

Stores the current transformation matrix.
The pushed transformation can be brought back using @racket[pop-matrix].
The transformation matrix is stored in a stack - hence the names @racket[push-matrix] and @racket[pop-matrix].

Use @racket[push-matrix] when you need temporarily to change the coordinate system. Use @racket[push-matrix]
before your changes, then use @racket[pop-matrix] afterwards to bring back the old setting.


@;---------

@subsubsection{pop-matrix}

@bold{Name: } @defidentifier[#'pop-matrix]

Restores a transformation matrix.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (code:comment "White rectangle")
          (fill 255)
          (rect 0 0 50 50)

          (code:comment "Black rectangle")
          (push-matrix)
          (translate 30 20)
          (fill 0)
          (rect 0 0 50 50)
          (pop-matrix)

          (code:comment "Gray rectangle")
          (fill 100)
          (eval:alts        (rect 15 10 50 50)
                     (begin (rect 15 10 50 50)
                            (send dc get-bitmap)))]



@bold{Description}

Restores a current transformation matrix.
Brings back a transformation formerly pushed to the stack by @racket[push-matrix].

Use the pair @racket[push-matrix] and @racket[pop-matrix] when you need temporarily to change the coordinate system.
Use @racket[push-matrix] before your changes, then use @racket[pop-matrix] afterwards to bring back the old setting.


@;---------
@;---------
@;---------

@subsection{Mouse}

@;---------

@subsubsection{mouse-button}

@bold{Name: } @defidentifier[#'mouse-button]

System variable holding the currently pressed mouse button.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Click within the image and press the left and right")
          (code:comment "mouse buttons to change the color of the rectangle.")

          (define (draw)
            (cond
              [(and mouse-pressed (eq? mouse-button 'left))  (fill 0)]
              [(and mouse-pressed (eq? mouse-button 'right)) (fill 255)]
              [else                                          (fill 127)])
            (rect 25 25 50 50))]



@bold{Usage}

@racketusage[mouse-button]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid['left]   "the left mouse was pressed")
               (list @racketid['middle] "the middle mouse was pressed")
               (list @racketid['right]  "the right mouse was pressed"))]



@bold{Description}

System variable holding the currently pressed mouse button.

The value of @racket[mouse-button] is only valid if @racket[mouse-pressed] is true.


@;---------

@subsubsection{mouse-pressed}

@bold{Name: } @defidentifier[#'mouse-pressed]

System variable, true if a mouse button is pressed.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Click within the image")
          (code:comment "to change the color of the rectangle.")

          (define (draw)
            (cond
              [mouse-pressed  (fill 0)]
              [else           (fill 255)])
            (rect 25 25 50 50))]


@bold{Usage}

@racketusage[mouse-pressed]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racket[#t] "some mouse button is pressed")
               (list @racket[#f] "no mouse button is pressed"))]



@bold{Description}

System variable, true if any mouse button is pressed.

Use @racket[mouse-button] to find out, which mouse button is pressed.


@;---------

@subsubsection{mouse-x}

@bold{Name: } @defidentifier[#'mouse-x]

System variable holding the current x-coordinate of the mouse.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move the mouse to move the line horizontally")

          (define (draw)
            (background 204)
            (line mouse-x 20 mouse-x 80))]


@bold{Usage}

@racketusage[mouse-x]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid[integer] "current mouse x-coordinate"))]



@bold{Description}

System variable holding the current x-coordinate of the mouse.

Note that Sketching can only track the mouse position when the
mouse pointer is over the current window. The default value of @racketid[mouse-x] is @racket[0],
so @racket[0] will be returned until the mouse moves in front of the sketch
window. (This typically happens when a sketch is first run.) Once the
mouse moves away from the window, @racketid[mouse-x] will continue to report its
most recent position.

@;---------

@subsubsection{mouse-y}

@bold{Name: } @defidentifier[#'mouse-y]

System variable holding the current y-coordinate of the mouse.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move the mouse to move the line vertically.")

          (define (draw)
            (background 204)
            (line 20 mouse-y 80 mouse-y))]


@bold{Usage}

@racketusage[mouse-y]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid[integer] "current mouse y-coordinate"))]



@bold{Description}

System variable holding the current y-coordinate of the mouse.

Note that Sketching can only track the mouse position when the
mouse pointer is over the current window. The default value of @racketid[mouse-y] is @racket[0],
so @racket[0] will be returned until the mouse moves in front of the sketch
window. (This typically happens when a sketch is first run.) Once the
mouse moves away from the window, @racketid[mouse-y] will continue to report its
most recent position.


@;---------

@subsubsection{pmouse-x}

@bold{Name: } @defidentifier[#'pmouse-x]

System variable holding the previous x-coordinate of the mouse.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move the mouse quickly to see the difference ")
          (code:comment "between the current and previous position.")

          (define (draw)
            (background 204)
            (line mouse-x 20 pmouse-x 80)
            (displayln (~a mouse-x " : " pmouse-x)))]


@bold{Usage}

@racketusage[pmouse-x]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid[integer] "the previous mouse x-coordinate"))]



@bold{Description}

System variable holding the previous x-coordinate of the mouse. This is the
value @racketid[mouse-x] had in the previous frame.

@;---------

@subsubsection{pmouse-y}

@bold{Name: } @defidentifier[#'pmouse-y]

System variable holding the previous y-coordinate of the mouse.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move the mouse quickly to see the difference ")
          (code:comment "between the current and previous position.")
          (define (draw)
            (background 204)
            (line 20 mouse-y  80 pmouse-y )
            (displayln (~a mouse-y " : " pmouse-y)))]


@bold{Usage}

@racketusage[pmouse-y]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid[integer] "the previous mouse y-coordinate"))]



@bold{Description}

System variable holding the previous y-coordinate of the mouse. This is the
value @racketid[mouse-y] had in the previous frame.


@;---------

@subsubsection{on-mouse-pressed}

@bold{Name: } @defidentifier[#'on-mouse-pressed]

If defined in your program, the event handler @racket[on-mouse-pressed]
is called each time the mouse is pressed.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Press a mouse button to toggle the  ")
          (code:comment "the color of the rectangle between black and white.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-mouse-pressed)
            (:= col (- 255 col)))]


@bold{Usage}

@racketusage[(define (on-mouse-pressed) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-mouse-pressed]
is called each time a mouse button is pressed.

@;---------

@subsubsection{on-mouse-released}

@bold{Name: } @defidentifier[#'on-mouse-released]

If defined in your program, the event handler @racket[on-mouse-released]
is called each time a mouse button is released.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Press and release mouse button to toggle the  ")
          (code:comment "the color of the rectangle between black and white.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-mouse-released)
            (:= col (- 255 col)))]


@bold{Usage}

@racketusage[(define (on-mouse-released) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-mouse-released]
is called each time a mouse button is released.


@;---------

@subsubsection{on-mouse-moved}

@bold{Name: } @defidentifier[#'on-mouse-moved]

If defined in your program, the event handler @racket[on-mouse-moved]
is called each time the mouse is moved.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move your mouse across the image")
          (code:comment "to change the color of the rectangle.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-mouse-moved)
            (:= col (+ col 5))
            (when (> col 255)
              (:= col 0)))]


@bold{Usage}

@racketusage[(define (on-mouse-moved) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-mouse-moved]
is called each time the mouse is moved.


@;---------

@subsubsection{on-mouse-dragged}

@bold{Name: } @defidentifier[#'on-mouse-dragged]

If defined in your program, the event handler @racket[on-mouse-dragged]
is called each time the mouse is dragged.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Drag (click and hold) your mouse across the ")
          (code:comment "image to change the color of the rectangle.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-mouse-dragged)
            (:= col (+ col 5))
            (when (> col 255)
              (:= col 0)))]


@bold{Usage}

@racketusage[(define (on-mouse-dragged) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-mouse-dragged]
is called each time the mouse is dragged. A mouse is dragged, when
it is moved while while a mouse button is pressed.


@;---------
@;---------
@;---------


@subsection{Keyboard}

@;---------

@subsubsection{key}

@bold{Name: } @defidentifier[#'key]

System variable holding the currently pressed key.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Click the sketch to give it focus.")
          (code:comment "Press b to change the color.")

          (define (draw)
            (cond
              [(and key-pressed (or (equal? key #\b) (equal? key #\B)))
               (fill 0)]
              [else
               (fill 255)])
            (rect 25 25 50 50))]



@bold{Usage}

@racketusage[key]        @linebreak[]



@bold{Description}

System variable holding the currently pressed key.

The value of @racket[key] is only valid if @racket[key-pressed] is true.


@;---------

@subsubsection{key-pressed}

@bold{Name: } @defidentifier[#'key-pressed]

System variable, true if some key is pressed.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Press any key.")
          (define (draw)
            (if key-pressed
                (fill 0)
                (fill 255))
            (rect 25 25 50 50))]


@bold{Usage}

@racketusage[key-pressed]        @linebreak[]


@bold{Description}

System variable, true if some key is pressed.

Use @racket[key] to find out, which key is pressed.


@;---------

@subsubsection{on-key-pressed}

@bold{Name: } @defidentifier[#'on-key-pressed]

If defined in your program, the event handler @racket[on-key-pressed]
is called each time a key is pressed.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Press a key to toggle the  ")
          (code:comment "the color of the rectangle between black and white.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-key-pressed)
            (:= col (- 255 col)))]


@bold{Usage}

@racketusage[(define (on-key-pressed) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-key-pressed]
is called each time some key is pressed.


@;---------

@subsubsection{on-key-released}

@bold{Name: } @defidentifier[#'on-key-released]

If defined in your program, the event handler @racket[on-key-released]
is called each time a is released.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Press and release a key to toggle the  ")
          (code:comment "the color of the rectangle between black and white.")

          (define col 0)

          (define (draw)
            (fill col)            
            (rect 25 25 50 50))
          
          (define (on-kyy-released)
            (:= col (- 255 col)))]


@bold{Usage}

@racketusage[(define (on-key-released) <body>)]        @linebreak[]


@bold{Description}


If defined in your program, the event handler @racket[on-key-released]
is called each time a key is released.


@;---------
@;---------
@;---------

@subsection{Math Operators}

@;---------

@subsubsection{+= (add assign)}

@bold{Name: } @defidentifier[#'+=]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 1)
          (+= a (+ 4 6))
          a]


@bold{Usage}

@racketusage[(+= id expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier")
               (list @racketid[expr]    "an expression"))]



@bold{Description}

The expression @racket[(+= id expr)] is equivalent to:

@racket[(begin
          (set! id (+ id expr))
          id)]

That is, @racket[(+= id expr)] computes the sum, stores it in @racketid[id]
and the returns the sum.

@;---------

@subsubsection{-= (subtract assign)}

@bold{Name: } @defidentifier[#'-=]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 10)
          (-= a (+ 2 1))
          a]


@bold{Usage}

@racketusage[(-= id expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier")
               (list @racketid[expr]    "an expression"))]


@bold{Description}

The expression @racket[(-= id expr)] is equivalent to:

@racket[(begin
          (set! id (- id expr))
          id)]

That is, @racket[(-= id expr)] computes the difference, stores it in @racketid[id]
and the returns the difference.


@;---------

@subsubsection{*= (multiply assign)}

@bold{Name: } @defidentifier[#'*=]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 2)
          (*= a 3)
          a]


@bold{Usage}

@racketusage[(*= id expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier")
               (list @racketid[expr]    "an expression"))]



@bold{Description}

The expression @racket[(*= id expr)] is equivalent to:

@racket[(begin
          (set! id (* id expr))
          id)]

That is, @racket[(*= id expr)] computes the product, stores it in @racketid[id]
and the returns the product.

@;---------

@subsubsection{/= (divide assign)}

@bold{Name: } @defidentifier[#'/=]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 10)
          (/= a 2)
          a]


@bold{Usage}

@racketusage[(/= id expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier")
               (list @racketid[expr]    "an expression"))]


@bold{Description}

The expression @racket[(/= id expr)] is equivalent to:

@racket[(begin
          (set! id (/ id expr))
          id)]

That is, @racket[(/= id expr)] computes the quotient stores it in @racketid[id]
and the returns the quotient.


@;---------

@subsubsection{++ (post increment)}

@bold{Name: } @defidentifier[#'++]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 10)
          (define b  1)
          (++ a)
          a
          (+ (++ b) 10)
          b]


@bold{Usage}

@racketusage[(++ id)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier"))]


@bold{Description}

The expression @racket[(++ id)] is equivalent to:

@racket[(begin
          (set! id (+ id 1))
          id)]

That is, @racket[(++ id)] adds one to @racketid[id], stores the new value,
and the returns the new value.

Note: This operation is part of Sketching, but not Racket.

Note: Adding one is called incrementing.

@;---------

@subsubsection{-- (post decrement)}

@bold{Name: } @defidentifier[#'--]



@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (define a 10)
          (define b  1)
          (-- a)
          a
          (+ (-- b) 10)
          b]


@bold{Usage}

@racketusage[(-- id)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[id]      "an identifier"))]


@bold{Description}

The expression @racket[(-- id)] is equivalent to:

@racket[(begin
          (set! id (- id 1))
          id)]

That is, @racket[(-- id)] subtracts one from @racketid[id], stores the new value,
and the returns the new value.

Note: This operation is part of Sketching, but not Racket.

Note: Subtracting one is called decrementing.


@;---------
@;---------
@;---------

@subsection{Math Functions}

@;---------

@subsubsection{@racket[abs] - Absolute Value }

@bold{Name}

@defidentifier[#'abs]

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (abs  1)
          (abs  0)
          (abs -1)]


@bold{Usage}

@racketusage[(abs x)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]    "a number"))]



@bold{Description}

The function @racket[abs] computes the absolute value.

The absolute value of a number @racketid[x] is the distance from @racketid[x] to @racket[0]
on the number line. One can think of @racket[(abs x)] as the "size" of @racketid[id]
since the sign is discarded.

@;---------

@subsubsection{@racket[ceil] - Ceiling }

@bold{Name}

@defidentifier[#'ceil]

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (ceil 2.6)
          (ceil 2.5)
          (ceil 2.4)
          (ceil -0.1)
          (ceil -0.4)
          (ceil -0.5)
          (ceil -0.6)]


@bold{Usage}

@racketusage[(ceil x)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]    "a number"))]



@bold{Description}

The function @racket[ceil] computes the "ceiling" of a number.

The ceiling of a number is the smallest integer that is at least as large as @racketid[x].

Think of @racket[ceil] as "rounding up" to nearest integer.

If we think of @racketid[x] as a number on the number line, @racketid[(ceil x)] is
the first integer larger than @racketid[x].

Note: In Racket the ceiling function is called @racket[ceiling] and not @racket[ceil].

@;---------

@subsubsection{@racket[constrain] - Constraining a value to an interval (clamping)}

@bold{Name}

@defidentifier[#'constrain]

@bold{Examples}


@examples[#:label #f #:eval se
          (code:comment "Constraining a value to the interval from 5 to 10.")
          (constrain 4 5 10)
          (constrain 5 5 10)
          (constrain 6 5 10)
          (code:comment "And from the other side")
          (constrain  9 5 10)
          (constrain 10 5 10)
          (constrain 11 5 10)]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Move the mouse to affect the horizontal position.")

          (define (draw)
            (background 204)
            (define mx (constrain mouse-x 30 70))
            (rect (- mx 10) 40 20 20))]


@bold{Usage}

@racketusage[(constrain x low high)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]     "a number")
               (list @racketid[low]   "a number, start of the interval")
               (list @racketid[high]  "a number, end of the interval"))]
               



@bold{Description}

The function @racket[constrain] "constrains" a value to a certain interval.

If @racketid[x] is a number between @racketid[low] and @racketid[high],
then @racketusage[(constrain x low high)] simply returns @racketid[x] unchanged.

If @racketid[x] is a number lower than @racketid[low], 
then @racketid[low] is returned.

If @racketid[x] is a number higher than @racketid[high], 
then @racketid[high] is returned.

The result of @racketusage[(constrain x low high)] is thus guaranteed to
lie between @racketid[low] and @racketid[high].


@;---------

@subsubsection{@racket[dist] - Distance between two points}

@bold{Name}

@defidentifier[#'dist]

@bold{Examples}


@examples[#:label #f #:eval se
          (code:comment "The lengths of the sides of a 3-4-5 triangle.")
          (dist 0 0 3 0)
          (dist 3 0 3 4)
          (dist 3 4 0 0)]

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se #:lang sketching
          (code:comment "Sets the background gray value based on the distance")
          (code:comment "from the mouse position to the center of the screen.")

          (define (draw)
            (no-stroke)
            (define d            (dist mouse-x mouse-y (/ width 2) (/ height 2)))
            (define max-distance (dist 0 0 (/ width 2) (/ height 2)))
            (define gray         (remap d 0 max-distance 0 255))
            (fill gray)
            (rect 0 0 width height))]


@bold{Usage}

@racketusage[(dist x1 y1 x2 y2)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]   "x-coordinate of the first point")
               (list @racketid[y1]   "y-coordinate of the first point")
               (list @racketid[x2]   "x-coordinate of the second point")
               (list @racketid[y2]   "y-coordinate of the second point"))]
               



@bold{Description}

Computes the distance from the point (@racketid[x1],@racketid[y1]) to (@racketid[x2],@racketid[y2]).


@;---------

@subsubsection{@racket[exp] - The natural exponential function}

@bold{Name}

@defidentifier[#'exp]

@bold{Examples}


@examples[#:label #f #:eval se          
          (exp 0)
          (exp 1)
          (exp 2)]
          

@bold{Usage}

@racketusage[(exp x)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number"))]
               



@bold{Description}

The natural exponential function @racket[exp] computes the power of
Eulers number, 2.718281828..., to the power of @racketid[x].

As a special case, @racketid[(exp 1)] returns Euler's number.

Normally the result is inexact, but for @racketid[x]=0 an
exact @racket[1] is returned.

@;---------

@subsubsection{@racket[floor] - Floor}

@bold{Name}

@defidentifier[#'floor]

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (floor 2.6)
          (floor 2.5)
          (floor 2.4)
          (floor -0.1)
          (floor -0.4)
          (floor -0.5)
          (floor -0.6)]


@bold{Usage}

@racketusage[(floor x)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]    "a number"))]



@bold{Description}

The function @racket[floor] computes the "floor" of a number.

The floor of a number is the largest integer that is no more than @racketid[x].

Think of @racket[floor] as "rounding down" to nearest integer.

If we think of @racketid[x] as a number on the number line, @racketid[(ceil x)] is
the first integer smaller than @racketid[x].

@;---------

@subsubsection{@racket[lerp] - Linear Interpolation}

@bold{Name}

@defidentifier[#'lerp]

@bold{Examples}


@examples[#:label #f #:eval se
          (code:comment "Linear interpolation from 10 to 20.")
          (lerp  10 20 0.0)
          (lerp  10 20 0.2)
          (lerp  10 20 0.4)
          (lerp  10 20 0.6)
          (lerp  10 20 0.8)
          (lerp  10 20 1.0)
          (code:comment "Values outside the range 0 to 1 works too.")
          (lerp  10 20 2.0)]


@examples[#:label #f #:eval se
          (stroke-weight 6)
          (define a 20)
          (define b 80)
          (define c (lerp a b .2))
          (define d (lerp a b .5))
          (define e (lerp a b .8))
          (point a 50)
          (point b 50)
          (point c 50)
          (point d 50)
          (eval:alts        (point e 50)
                     (begin (point e 50)
                            (send dc get-bitmap)))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@bold{Usage}

@racketusage[(lerp start stop t)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[t]    "a number from 0 to 1"))]



@bold{Description}

Linearly interpolate between the numbers @racketid[start] and @racketid[stop].

The number returned by @racketusage[(lerp start stop t)] is @racketid[(+ start (* t (- stop start)))].

For values of t between 0 and 1, one can think of @racket[lerp] as map from the interval from 0 to 1
onto the interval from @racketid[start] to @racketid[stop].

The lerp function is convenient for creating motion along a straight path and for drawing dotted lines.


@;---------

@subsubsection{@racket[log] - Logarithms}

@bold{Name}

@defidentifier[#'log]

@bold{Examples}


@examples[#:label #f #:eval se          
          (log 1)
          (log 2)
          (log 8 2)
          (log (exp 1))
          (log (exp 2))]
          

@bold{Usage}

@racketusage[(log x)]          @linebreak[]
@racketusage[(log x b)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a positive number")
               (list @racketid[x]   "the base of the logarithm (defaults to e)"))]
               



@bold{Description}

The natural logarithm function @racket[log] computes the
base @racketid[b] logarithm of @racketid[x]. The base default
to Eulers number, 2.718281828. That is, the one argument
version of @racket[log] is the natural logarithm.


Normally the result is inexact, but for @racketid[1]=0 an
exact @racket[0] is returned.


@;---------

@subsubsection{@racket[mag] - Vector Magnitude}

@bold{Name}

@defidentifier[#'mag]

Computes the magnitude of a vector.

@bold{Examples}

@examples[#:label #f #:eval se          
          (mag 3 4)]
          

@bold{Usage}

@racketusage[(mag x y)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "the x-coordinate")
               (list @racketid[y]   "the y-coordinate"))]


@bold{Description}

The function @racketusage[(mag x y)] computes the magnitude (length) of the vector (@racketid[x],@racketid[y]).

The formula used to compute the magnitude is @racket[(sqrt (+ (* x x) (* y y)))].


@;---------

@subsubsection{@racket[remap] - Convert from one range to another}

@bold{Name}

@defidentifier[#'remap]

Maps a value in one range into another range.

@bold{Examples}

@examples[#:label #f #:eval se
          (code:comment "The number 15 in the range 10 to 20 maps to 150 in the range 100 to 200.")
          (remap 15 10 20 100 200)
          (code:comment "Values aren't clamped to the interval")
          (remap 5 10 20 100 200)]
          

@bold{Usage}

@racketusage[(remap x from1 to1 from2 start2)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]     "a number")
               (list @racketid[from1] "start of the first interval")
               (list @racketid[to1]   "end of the first interval")
               (list @racketid[from2] "start of the second interval")
               (list @racketid[to2]   "end of the second interval"))]


@bold{Description}

The function @racket[remap] maps a value @racketid[x] in one interval 
[@racketid[from1];@racketid[to1]] into another interval [@racketid[from2];@racketid[to2]].

          
@;---------

@subsubsection{@racket[max] - Maximum}

@bold{Name}

@defidentifier[#'max]

Computes the maximum of one or more values.

@bold{Examples}

@examples[#:label #f #:eval se
          (max 0 1)
          (max 0 1 2)]
          

@bold{Usage}

@racketusage[(max x1 x2 ...)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]     "a number")
               (list @racketid[x2 ...] "zero or more numbers"))]


@bold{Description}

Computes the maximum of one or more numbers.


@;---------

@subsubsection{@racket[min] - Minimum}

@bold{Name}

@defidentifier[#'min]

Computes the minimum of one or more values.

@bold{Examples}

@examples[#:label #f #:eval se
          (min 0 1)
          (min 0 1 2)]
          

@bold{Usage}

@racketusage[(min x1 x2 ...)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x1]     "a number")
               (list @racketid[x2 ...] "zero or more numbers"))]


@bold{Description}

Computes the minimum of one or more numbers.

@;---------

@subsubsection{@racket[norm] - Normalize number}

@bold{Name}

@defidentifier[#'norm]

Normalizes a number from another range into a value between 0 and 1. 

@bold{Examples}

@examples[#:label #f #:eval se
          (norm 20. 0 50)
          (norm -10. 0 100)]
          

@bold{Usage}

@racketusage[(norm x start end)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]     "number to be normalized")
               (list @racketid[start] "start of interval")
               (list @racketid[ned]   "end of interval"))]


@bold{Description}

Normalizes a number from a range into a value between 0 and 1.
Identical to @racketusage[(remap x start end 0 1)].

Numbers outside of the range are not clamped to 0 and 1,
because out-of-range values are often intentional and useful. 


@;---------

@subsubsection{@racket[pow] - Powers}

@bold{Name}

@defidentifier[#'pow]

Computes powers of a number.

@bold{Examples}

@examples[#:label #f #:eval se
          (pow  1  3)
          (pow  2  3)
          (pow  2 -3)
          (pow -2  3)]
          

@bold{Usage}

@racketusage[(pow x y)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "base number")
               (list @racketid[y]   "exponent"))]


@bold{Description}

Computes the power @racketid[x] to @racketid[y].

Note: In Racket we normally use @racket[expt] (short for exponentiate) 
instead of @racket[pow].


@;---------

@subsubsection{@racket[round] - Round to nearest integer}

@bold{Name}

@defidentifier[#'round]

Rounds to nearest integer.

@bold{Examples}

@examples[#:label #f #:eval se
          (round 0.4)
          (round 0.5)
          (round 0.6)
          (round 1.4)
          (round 2.5)
          (round 3.6)]
          

@bold{Usage}

@racketusage[(round x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number"))]


@bold{Description}

Rounds @racketid[x] to nearest integer.

Note: This is the standard Racket @racket[round] that for numbers ending in .5
rounds to even following the recommendations of the IEEE floating point standard.

If you need a rounding function that always rounds towards plus infinity, then
use this definition in your program:

@examples[#:label #f #:eval se
(code:comment "round ties towards +inf.0")
(define (round-up x)
  (floor (+ x 0.5)))]

@;---------

@subsubsection{@racket[sq] - Squaring}

@bold{Name}

@defidentifier[#'sq]

Computes the square of a number.

@bold{Examples}

@examples[#:label #f #:eval se
(sq 2)
(sq 3)]


@bold{Usage}

@racketusage[(sq x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number"))]


@bold{Description}

For a number @racketid[x] computes the square @racket[(* x x)] of the number.

@;---------

@subsubsection{@racket[sqrt] - Square Root}

@bold{Name}

@defidentifier[#'sqrt]

Computes the square root of a number.

@bold{Examples}

@examples[#:label #f #:eval se
(sqrt 2)
(sqrt 4)
(sqrt 16)]


@bold{Usage}

@racketusage[(sqrt x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number"))]


@bold{Description}

Computes the square root of the number.


@;---------


@;---------
@;---------
@;---------

@subsection{Trigonometry}

@;---------

@subsubsection{@racket[cos] - Cosine}


@bold{Name}

@defidentifier[#'cos]

Computes the cosine of an angle in radians.

@bold{Examples}

@examples[#:label #f #:eval se
(cos 0)
(cos (/ π 2))
(cos (radians 90))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 360 100))
          (fill 196) (no-stroke) (rect 0 0 360 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 360 100)
(eval:alts
 (for ([d (in-range 0 360 4)]) 
   (line d 50 d (+ 50 (* 40 (cos (radians d))))))
 (begin
   (for ([d (in-range 0 360 4)]) 
     (line d 50 d (+ 50 (* 40 (cos (radians d))))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(cos x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "an angle in radians"))]


@bold{Description}

Computes the cosine of angle @racketid[x] measured in radians.

Note: Use @racket[radians] to convert an angle in degrees to radians.


@;---------

@subsubsection{@racket[sin] - Sine}

@bold{Name}

@defidentifier[#'sin]

Computes the sine of a number in radians.

@bold{Examples}

@examples[#:label #f #:eval se
(sin 0)
(sin (/ π 2))
(sin (radians 90))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 360 100))
          (fill 196) (no-stroke) (rect 0 0 360 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 360 100)
(eval:alts
 (for ([d (in-range 0 360 4)]) 
   (line d 50 d (+ 50 (* 40 (sin (radians d))))))
 (begin
   (for ([d (in-range 0 360 4)]) 
     (line d 50 d (+ 50 (* 40 (sin (radians d))))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(sin x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "an angle in radians"))]


@bold{Description}

Computes the sine of angle @racketid[x] measured in radians.

Note: Use @racket[radians] to convert an angle in degrees to radians.

@;---------

@subsubsection{@racket[tan] - Tangent}

@bold{Name}

@defidentifier[#'tan]

Computes the tangent of a number in radians.

@bold{Examples}

@examples[#:label #f #:eval se
(tan 0)
(tan (/ π 4))
(tan (radians 45))]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 360 300))
          (fill 196) (no-stroke) (rect 0 0 360 300) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 360 300)
(eval:alts
 (for ([d (in-range 0 360 4)]) 
   (line d 150 d (+ 150 (* 40 (tan (radians d))))))
 (begin
   (for ([d (in-range 0 360 4)]) 
     (line d 150 d (+ 150 (* 40 (tan (radians d))))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(tan x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "an angle in radians"))]


@bold{Description}

Computes the tangent of angle @racketid[x] measured in radians.

Note: Use @racket[radians] to convert an angle in degrees to radians.


@;---------

@subsubsection{@racket[acos] - Inverse Cosine}

@bold{Name}

@defidentifier[#'acos]

Computes the inverse cosine.

@bold{Examples}

@examples[#:label #f #:eval se
(acos -1)
(acos 0)
(acos 1)]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 200 180))
          (fill 196) (no-stroke) (rect 0 0 200 180) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 200 180)
(code:comment "Put (0,0) in the lower, left corner")
(translate 100 180)
(code:comment "Make the y-axis point up")
(scale 1 -1)
(code:comment "Plot acos")
(eval:alts
 (for ([x (in-range -100 104 4)])
   (line x 0 x  (degrees (acos (/ x 100.)))))
 (begin
   (for ([x (in-range -100 104 4)])
   (line x 0 x  (degrees (acos (/ x 100.)))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(acos x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number between -1 and 1 (inclusive)"))]


@bold{Description}

Computes the inverse cosine of a number. The result is in radians.

Note: Use @racket[degrees] to convert an angle in radians to degrees.


@;---------

@subsubsection{@racket[asin] - Inverse Sine}

@bold{Name}

@defidentifier[#'asin]

Computes the inverse sine.

@bold{Examples}

@examples[#:label #f #:eval se
(asin -1)
(asin 0)
(asin 1)]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 200 180))
          (fill 196) (no-stroke) (rect 0 0 200 180) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 200 180)
(code:comment "Put (0,0) in the center of the screen")
(translate 100 90)
(code:comment "Make the y-axis point up")
(scale 1 -1)
(code:comment "Plot asin")
(eval:alts
 (for ([x (in-range -100 104 4)])
   (line x 0 x  (degrees (asin (/ x 100.)))))
 (begin
   (for ([x (in-range -100 104 4)])
     (line x 0 x  (degrees (asin (/ x 100.)))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(asin x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number between -1 and 1 (inclusive)"))]


@bold{Description}

Computes the inverse sine of a number. The result is in radians.

Note: Use @racket[degrees] to convert an angle in radians to degrees.


@;---------

@subsubsection{@racket[atan] - Inverse Tangent}

@bold{Name}

@defidentifier[#'atan]

Computes the inverse tangent.

@bold{Examples}

@examples[#:label #f #:eval se
(atan -1/2)
(atan 0)
(atan 1/2)]


@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 400 180))
          (fill 196) (no-stroke) (rect 0 0 400 180) (stroke 0) (color-mode 'rgb 255) (fill 255)]


@examples[#:label #f #:eval se
(size 400 180)
(code:comment "Put (0,0) in the center of the screen")
(translate 200 90)
(code:comment "Make the y-axis point up")
(scale 1 -1)
(code:comment "Plot atan")
(eval:alts
 (for ([x (in-range -200 204 4)])
   (line x 0 x  (degrees (atan (/ x 100.)))))
 (begin
   (for ([x (in-range -200 204 4)])
     (line x 0 x  (degrees (atan (/ x 100.)))))
   (send dc get-bitmap)))]


@bold{Usage}

@racketusage[(atan x)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[x]   "a number between -1 and 1 (inclusive)"))]


@bold{Description}

Computes the inverse tangent of a number. The result is in radians.

Note: Use @racket[degrees] to convert an angle in radians to degrees.


@;---------

@subsubsection{@racket[atan2] - Inverse Tangent}

@bold{Name}

@defidentifier[#'atan2]

Computes the angle between the positive part of the @racketid[x]-axis and the 
line segment from (@racketid[0],@racketid[0]) to (@racketid[x],@racketid[y]).

@bold{Examples}

@examples[#:label #f #:eval se
(degrees (atan2  0  1))
(degrees (atan2  1  1))
(degrees (atan2  1  0))
(degrees (atan2  1 -1))
(degrees (atan2  0 -1))]



@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (size 200 200)
  (define w/2 (/ width 2))
  (define h/2 (/ height 2))
  (code:comment "Put (0,0) in the center of the screen")
  (translate w/2 h/2)
  (code:comment "Make the y-axis point up")
  (scale 1 -1))
(define (draw)
  (code:comment "Angle to mouse")
  (define a (atan2 (- mouse-y h/2) (- mouse-x w/2)))
  (rotate a)
  (rect -30 -5 60 10))]


@bold{Usage}

@racketusage[(atan2 y x)]          @linebreak[]

@tabular[#:sep @hspace[1]
(list (list @racketid[y]   "y-coordinate")
      (list @racketid[x]   "x-coordinate"))]


@bold{Description}

Computes the angle between the positive part of the @racketid[x]-axis and the 
line segment from (@racketid[0],@racketid[0]) to (@racketid[x],@racketid[y]).

Note that @racket[atan2] has the @racketid[y]-coordinate first.

The resulting angle is in radians. Use @racket[degrees] if you need
to convert the angle to degrees.

@;---------

@subsubsection{@racket[radians] - Convert Degrees to Radians}

@bold{Name}

@defidentifier[#'radians]

Converts an angle in degrees to radians.

@bold{Examples}

@examples[#:label #f #:eval se
(radians   0)
(radians  90)
(radians 180)
(radians 360)]


@bold{Usage}

@racketusage[(radians deg)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[deg]   "a number"))]


@bold{Description}

Converts an angle in degrees to radians.

Radians and degrees are two ways of measuring the same
thing. There are 360 degrees in a circle and 2pi radians in a
circle. 

All trigonometric functions in Sketching require their parameters to be specified in radians.

@;---------

@subsubsection{@racket[degrees] - Convert Radians to Degrees}

@bold{Name}

@defidentifier[#'degrees]

Converts an angle in radiands to degrees.

@bold{Examples}

@examples[#:label #f #:eval se
(degrees   0)
(degrees  (/ π 2))
(degrees  pi)
(degrees (* 2 π))]


@bold{Usage}

@racketusage[(degrees rad)]          @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[rad]   "a number"))]


@bold{Description}

Converts an angle in radians to degrees.

Radians and degrees are two ways of measuring the same
thing. There are 360 degrees in a circle and 2pi radians in a
circle. 

All trigonometric functions in Sketching require their parameters to be specified in radians.


@;----------------------
@;-- Math Conversion  --
@;----------------------

@subsection{Math Conversion}

@;---------

@subsubsection{int}

@bold{Name: } @defidentifier[#'int]

Converts a value to an integer.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (int 1)
          (int 1.4)
          (int 1.5)
          (int 3/2)
          (int #\a)
          (int #f)
          (int #t)]

@bold{Usage}

@racketusage[(int expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts numbers, characters and booleans into an exact integer.
Non-integer numbers are floored before they are converted.


@;---------

@subsubsection{char}

@bold{Name: } @defidentifier[#'char]

Converts an integer to a character.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (char 97)
          (int #\a)]

@bold{Usage}

@racketusage[(char expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts an integer to a character.

@;---------

@subsubsection{binary}

@bold{Name: } @defidentifier[#'binary]

Converts a value to string with the equivalent binary notation.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (binary 97)
          (binary #\a)
          (binary (color 0 255 0))]
          


@bold{Usage}

@racketusage[(binary expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts an integer, a character or a color to a string containing 
the equivalent binary notation.


@;---------

@subsubsection{unbinary}

@bold{Name: } @defidentifier[#'unbinary]

Converts a string containing a binary number into the corresponding integer.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (binary 13)
          (unbinary "1101")]


@bold{Usage}

@racketusage[(unbinary expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts a string containing a binary number into the corresponding integer.


@;---------

@subsubsection{hex}

@bold{Name: } @defidentifier[#'hex]

Converts integers, characters and colors to a hexadecimal string.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (hex 97)
          (hex #\a)
          (hex (color 0 255 0))]

@bold{Usage}

@racketusage[(hex expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts integers, characters and colors to a hexadecimal string.

@;---------

@subsubsection{unhex}

@bold{Name: } @defidentifier[#'unhex]

Converts a hexadecimal string to an integer.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se
          (hex 97)
          (unhex "61")]

@bold{Usage}

@racketusage[(unhex expr)]        @linebreak[]

@tabular[#:sep @hspace[1]
         (list (list @racketid[expr]    "an expression"))]



@bold{Description}

Converts a hexadecimal string to an integer.


@;---------
@;---------
@;---------

@subsection{Constants}

@;---------

@subsubsection{Pi and friends}


@bold{Name}

@defidentifier[#'pi]   , @defidentifier[#'π],
@defidentifier[#'pi/2] , @defidentifier[#'π/2],
@defidentifier[#'pi/4] , @defidentifier[#'π/4],
@defidentifier[#'2pi]  , @defidentifier[#'2π]



@bold{Examples}

@examples[#:label #f #:eval se
(list pi π)
(list pi/2 π/2)
(list pi/4 π/4)
(list 2pi 2π)]


@examples[#:hidden #:eval se
(current-dc (new-bitmap-dc 100 100))
(size 100 100) (reset-matrix)
(color-mode 'rgb 255)
(fill 196) (no-stroke) (rect 0 0 100 100)
(stroke 0) (fill 255)]

@examples[#:label #f #:eval se
(size 100 100)
(define x (/ width  2))
(define y (/ height 2))
(define d (* 0.8 width))
(ellipse-mode 'center)
(arc x y     d         d      0  π/4)
(arc x y  (- d 20)  (- d 20)  0  π/2)
(arc x y  (- d 40)  (- d 40)  0  π)
(arc x y  (- d 60)  (- d 60)  0  2π)
(eval:alts
 (arc x y  (- d 60)  (- d 60)  0  2π) 
 (begin
   (arc x y  (- d 60)  (- d 60)  0  2π)
   (send dc get-bitmap)))]


@bold{Description}

The mathematical constant @racket[π] is the number 3.1415927.
It is the ratio of the circumference of a circle to its diameter. 
It is useful in combination with the trigonometric functions @racket[sin] and @racket[cos].

@;---------
@;---------
@;---------

@subsection{Environment}

@;---------

@;---------

@subsubsection{width}

@bold{Name: } @defidentifier[#'width]

System variable whose value is the width of the canvas.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
(no-stroke)
(background 0)
(rect 0 40    width    20)
(eval:alts
 (rect 0 60 (/ width 2) 20)
 (begin (rect 0 60 (/ width 2) 20)
        (send dc get-bitmap)))]


@bold{Usage}

@racketusage[width]            @linebreak[]


@bold{Description}

The system variable @racket[width] holds the width of the canvas.

The variable is initially set by the @racket[size] function from within @racket[setup].
If @racket[size] is not called by @racket[setup], the default width is 100.

It is not possible to use @racket[set!] to change the width of the canvas.


@;---------

@subsubsection{height}

@bold{Name: } @defidentifier[#'height]

System variable whose value is the height of the canvas.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
(no-stroke)
(background 0)
(rect 40 0    height    20)
(eval:alts
 (rect 60 0 (/ height 2) 20)
 (begin (rect 60 0 (/ height 2) 20)
        (send dc get-bitmap)))]


@bold{Usage}

@racketusage[height]            @linebreak[]


@bold{Description}

The system variable @racket[height] holds the height of the canvas.

The variable is initially set by the @racket[size] function from within @racket[setup].
If @racket[size] is not called by @racket[setup], the default height is 100.

It is not possible to use @racket[set!] to change the height of the canvas.


@;---------

@subsubsection{size}

@bold{Name: } @defidentifier[#'size]

Sets the size of the canvas (window).

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 200 100))
          (fill 196) (no-stroke) (rect 0 0 200 100) (stroke 0)]


@examples[#:label #f #:eval se
(size 200 100)
(background 153)
(eval:alts
 (line 0 0 width height)
 (begin (line 0 0 width height)
        (send dc get-bitmap)))]


@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (size 320 240))

(define (draw)
  (background 153)
  (line 0 0 width height))]


@bold{Usage}

@racketusage[(size w h)]            @linebreak[]

@tabular[#:sep @hspace[1]
(list (list @racketid[w]   "an integer width")
      (list @racketid[h]   "an integer height"))]


@bold{Description}

Defines the dimension of the display window width and height in units
of pixels. Use @racket[size] as the first expression in your
@racket[setup] function.

The built-in variables width and height are set by the values
passed to this function. For example, running @racket[(size 640 480)] will
assign 640 to the width variable and 480 to the height variable. If
@racket[size] is not used, the window will be given a default size of
100 × 100 pixels.

The @racket[size] function can only be used once inside a sketch, and it
cannot be used for resizing.

If you need a full screen canvas, use @racket[full-screen].

The maximum width and height is limited by your operating system, and
is usually the width and height of your actual screen. On some
machines it may simply be the number of pixels on your current screen.

The minimum width and height is around 100 pixels in each direction.
This is the smallest that is supported across Windows, macOS, and Linux


@;---------

@subsubsection{fullscreen}

@bold{Name: } @defidentifier[#'fullscreen]

Use the full screen for the canvas.

@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(define x 0)

(define (setup)
  (fullscreen)
  (background 0)
  (no-stroke)
  (fill 102))

(define (draw)
  (rect x (* 0.2 height) 1 (* 0.6 height))
  (:= x (+ x 2)))]


@bold{Usage}

@racketusage[(fullscreen)]            @linebreak[]


@bold{Description}

Use the full screen for the canvas.
Call @racket[fullscreen] from @racket[setup].

@;---------

@subsubsection{cursor}

@bold{Name: } @defidentifier[#'cursor]

Sets the image used as the mouse cursor.

@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (cursor 'arrow))]

@examples[#:label #f #:eval se #:lang sketching
(code:comment "Move the mouse pointer to see the different mouse cursors")
(define cursors '(arrow bullseye cross hand ibeam watch blank
                        size-n/s size-e/w size-ne/sw size-nw/se))

(define (setup)
  (size 500 100)
  (cursor 'arrow))

(define (draw)
  (code:comment "draw vertical strips")
  (define n (length cursors))
  (define w (/ width n))
  (stroke 255)
  (for ([x (in-range 0 width w)])
    (fill (floor (lerp 0 255 (/ x width))))
    (rect x 0 w height))
  (code:comment "change cursor")
  (define index (constrain (floor (/ mouse-x w)) 0 (- n 1)))
  (define c     (list-ref cursors index))
  (code:comment "write chosen cursor")
  (text-size 30)
  (fill 255)
  (text (~a c) 200 25)
  (cursor c))]
  
  


@bold{Usage}

@racketusage[(cursor sym)]            @linebreak[]
@racketusage[(cursor bm)]            @linebreak[]

@tabular[#:sep @hspace[1]
(list (list @racketid[sym] 
            (~a "one of the symbols 'arrow "
                "'bullseye 'cross 'hand 'ibeam 'watch 'blank "
                "'size-n/s 'size-e/w 'size-ne/sw 'size-nw/se"))
      (list @racketid[img]   "a bitmap"))]



@bold{Description}

Sets the image used as the mouse cursor.

Use @racketusage[(cursor sym)] to choose one of the builtin mouse cursors.

Use @racketusage[(cursor bm)] to use a bitmap as a custom mouse cursor.


@;---------

@subsubsection{no-cursor}

@bold{Name: } @defidentifier[#'no-cursor]

Hides the mouse cursor.

@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(code:comment "Press the mouse to hide the cursor")
(define (draw)
  (if mouse-pressed
      (no-cursor)
      (cursor 'hand)))]


@bold{Usage}

@racketusage[(no-cursor)]            @linebreak[]


@bold{Description}

Hides the mouse cursor.

Note: Currently there is a problem with the example on macOS Big Sur.
Mail me: does it work on Linux and Windows?

@;---------

@subsubsection{smoothing}

@bold{Name: } @defidentifier[#'smoothing]

Enables or disables anti-aliased smoothing for drawing.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0)]

@examples[#:label #f #:eval se
  (smoothing 'smoothed)
  (line 0 0 100 100)
  (smoothing 'unsmoothed)
  (eval:alts
   (line 100 100 0 0)
   (begin (line 100 0 0 100)
          (send dc get-bitmap)))]



@bold{Usage}

@racketusage[(smoothing sym)]            @linebreak[]

@tabular[#:sep @hspace[1]
(list (list @racketid[sym] 
            (~a "one of the symbols 'unsmoothed 'smoothed 'aligned ")))]



@bold{Description}

Enables or disables anti-aliased smoothing for drawing.
Text smoothing is not affected by this setting.

Use @racketusage[(smoothing 'unsmoothed)] to disable both anti-alias and pixel alignment.

Use @racketusage[(smoothing 'smoothed)]   to enable both anti-alias and pixel alignment.

Use @racketusage[(smoothing 'aligned)]    to disable anti-alias and enable pixel alignment.

See the Racket documentation for more on pixel alignment.


@subsubsection{no-smooth}

@bold{Name: } @defidentifier[#'no-smooth]

Disables anti-aliased smoothing for drawing.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255)]

@examples[#:label #f #:eval se
          (fill 255)
          (no-smooth)
          (eval:alts        (line 0 0 100 100)
                     (begin (line 0 0 100 100) (send dc get-bitmap)))]

@bold{Usage}

@racketusage[(no-smooth)]            @linebreak[]


@bold{Description}

Enables or disables anti-aliased smoothing for drawing.
Text smoothing is not affected by this setting.



@;---------

@subsubsection{nap}

@bold{Name: } @defidentifier[#'nap]

Pauses the program (the current thread).


@bold{Usage}

@racketusage[(nap ms)]            @linebreak[]


@tabular[#:sep @hspace[1]
(list (list @racketid[ms]   "milliseconds to pause"))]


@bold{Description}

Pauses the program (the current thread) for the given number of milliseconds.

Note: It's a bad practise to call @racket[nap] from @racket[draw] and 
the event handlers. If you need to affect the speed of an animation,
use @racket[frame-rate] instead.

Note: In processing the @racket[nap] function is called @defidentifier[#'delay].
In Racket @racket[delay] is used to make promisses instead.


@;---------

@subsubsection{focused}

@bold{Name: } @defidentifier[#'focused]

System variable holding whether the canvas has mouse and keyboard focus.

@bold{Examples}

@examples[#:hidden #:eval se
          (current-dc (new-bitmap-dc 100 100))
          (fill 196) (no-stroke) (rect 0 0 100 100) (stroke 0) (color-mode 'rgb 255) (fill 255)]

@examples[#:label #f #:eval se #:lang sketching
(define (draw)
  (cond
    [focused (ellipse 25 25 50 50)]
    [else    (line   0 0 100 100)
             (line 100 0   0 100)]))]



@bold{Usage}

@racketusage[focused]        @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racket[#t] "the canvas has focus")
               (list @racket[#f] "the canvas haven't got focus"))]



@bold{Description}

System variable holding whether the canvas has mouse and keyboard focus.

When the canvas has focus, keyboard events mouse events are sent sent to the
Sketching program. 


@;---------

@subsubsection{frame-count}

@bold{Name: } @defidentifier[#'frame-count]

The system variable @racket[frame-count] contains the number of frames
displayed since the program started.


@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (set-frame-rate! 30)
  (rect-mode 'center))

(define (draw)
  (background 0)
  (fill 255)
  (text (~a frame-count) 50 50))]



@bold{Usage}

@racketusage[frame-count]            @linebreak[]


@bold{Description}

The system variable @racket[frame-count] contains the number of frames
displayed since the program started. At the time @racket[setup]
is called, the value is 0. After each call to draw, the variable
is incremented.


@;---------

@subsubsection{frame-rate}

@bold{Name: } @defidentifier[#'frame-rate]

The system variable @racket[frame-rate] contains the approximate frame rate.


@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (set-frame-rate! 30))

(define (draw)
  (line 0 0 width height)
  (text (~a frame-rate) 40 50))]



@bold{Usage}

@racketusage[frame-rate]            @linebreak[]


@bold{Description}

The system variable @racket[frame-rate] contains the approximate frame rate.
The initial frame rate is 10 frames per second. The system variable is updated
with each frame. The value is averaged over several frames, so the value is
accurate only after the first 5-10 frames have been drawn.

Use @racket[set-frame-rate!] to set the frame rate to another value.

@;---------

@subsubsection{set-frame-rate!}

@bold{Name: } @defidentifier[#'set-frame-rate!]

Sets the desired number of frames to be displayed per second.


@bold{Examples}

@examples[#:label #f #:eval se #:lang sketching
(define (setup)
  (set-frame-rate! 30))

(define (draw)
  (line 0 0 width height)
  (text (~a frame-rate) 40 50))]



@bold{Usage}

@racketusage[(set-frame-rate! fps)]            @linebreak[]

@bold{Values}

@tabular[#:sep @hspace[1]
         (list (list @racketid[fps] "number of desired frames per second"))]


@bold{Description}

Sets the desired number of frames to be displayed per second.

The system will attempt to call @racket[draw] the desired number
of times per second, but there is no guarantees. If @racket[draw] is slow,
the desired number of frames per second might be achievable.


@;-------------------
@;- OTHER
@;-------------------

@subsection{Other}

@;---------

@subsubsection{loop}

@bold{Name: } @defidentifier[#'loop]

Start the draw loop.

@bold{Examples}

In this example, @racketusage[draw] is only called in the animation loop if
the mouse is pressed.

@codeblock{
#lang sketching

(define (setup)
  (size 200 200)
  (no-loop))

(define x 0)

(define (draw)
  (background 204)
  (+= x 1)
  (when (> x width)
    (:= x 0))
  (line x 0 x height))

(define (on-mouse-pressed)
  (loop))

(define (on-mouse-released)
  (no-loop))}
  

@bold{Usage}

@racketusage[(loop)]            @linebreak[]


@bold{Description}

Running a Sketching program will start an animation loop that
for each frame calls @racket[draw] which is expected to draw the next
frame (image) of the animation. For programs that display only
static images or react only to key and mouse events, there are no
reason to call @racket[draw] for every frame. The pair @racket[no-loop]
and @racket[loop] respectively disables and enables calling @racket[draw]
each frame.


@;---------

@subsubsection{no-loop}

@bold{Name: } @defidentifier[#'no-loop]

Makes the animation loop stop calling @racket[draw].

@bold{Examples}

In this example, @racketusage[draw] is only called in the animation loop if
the mouse is pressed.

@codeblock{
#lang sketching

(define (setup)
  (size 200 200)
  (no-loop))

(define x 0)

(define (draw)
  (background 204)
  (+= x 1)
  (when (> x width)
    (:= x 0))
  (line x 0 x height))

(define (on-mouse-pressed)
  (loop))

(define (on-mouse-released)
  (no-loop))}
  

@bold{Usage}

@racketusage[(no-loop)]            @linebreak[]


@bold{Description}

Running a Sketching program will start an animation loop that
for each frame calls @racket[draw] which is expected to draw the next
frame (image) of the animation. For programs that display only
static images or react only to key and mouse events, there are no
reason to call @racket[draw] for every frame. The pair @racket[no-loop]
and @racket[loop] respectively disables and enables calling @racket[draw]
each frame.


@;-------------------
@;- EXAMPLES
@;-------------------

@section{Examples}

This section contains more elaborate examples of how to use Sketching.

All examples are available at @|sketching-manual-examples-github|.

If you find any mistakes in the examples, please make an Github issue at @|sketching-github|.

I encourage you to submit your own examples.

If you lack inspiration, I'd love some help porting the examples on
@hyperlink["https://processing.org/examples/"]{Processing Examples}.
The source for these examples can be found at
@hyperlink["https://github.com/processing/processing-docs/tree/master/content/examples"]{
Processing examples at Github}.

The examples are divided into topics:

@local-table-of-contents[#:style 'immediate-only]


@subsection[#:tag "examples_color"]{Color}

The color examples are:

@local-table-of-contents[#:style 'immediate-only]

@subsubsection[#:tag "example_hue"]{Hue}

In color theory the concepts hue, saturation and brightness
are used characterize the color. A hue such as red, yellow, etc.
is what we in everyday language think of, when we hear the word "color".

Move the cursor vertically over each bar to alter its hue.

@(example-from-file "basics/color/hue.rkt")

@subsubsection[#:tag "example_saturation"]{Saturation}

Saturation is the strength or purity of the color.
One can think of saturation as the amount of gray in proportion to the hue.
A "saturated" color is pure and an "unsaturated" color has a large component of gray.

Move the cursor vertically over each bar to alter its saturation.

@(example-from-file "basics/color/saturation.rkt")

@subsubsection[#:tag "example_brightness"]{Brightness}

This program adjusts the brightness of a part of the image by
calculating the distance of each pixel to the mouse.

@(example-from-file "basics/color/brightness.rkt")

@subsubsection[#:tag "example_color_variables"]{Color Variables}
@(example-from-file "basics/color/color-variables.rkt")

@subsubsection[#:tag "example_relativity"]{Relativity}
@(example-from-file "basics/color/relativity.rkt")

@subsubsection[#:tag "example_linear_gradient"]{Linear Gradient}
@(example-from-file "basics/color/linear-gradient.rkt")

@subsubsection[#:tag "example_radial_gradient"]{Radial Gradient}
@(example-from-file "basics/color/radial-gradient.rkt")

@subsubsection[#:tag "example_wave_gradient"]{Wave Gradient}
@(example-from-file "basics/color/wave-gradient.rkt")


@subsection[#:tag "examples_classes_and_objects"]{Classes and Objects}

The examples are:

@local-table-of-contents[#:style 'immediate-only]


@subsubsection[#:tag "example_objects"]{Objects}
@(example-from-file "basics/objects/objects.rkt")

@subsubsection[#:tag "example_multiple_constructors"]{Multiple Constructors}
@(example-from-file "basics/objects/multiple-constructors.rkt")

@subsubsection[#:tag "example_composite_objects"]{Composite Objects}
@(example-from-file "basics/objects/composite-objects.rkt")

@subsubsection[#:tag "example_inheritance"]{Inheritance}
@(example-from-file "basics/objects/inheritance.rkt")


@subsection[#:tag "examples_input"]{Input}

The input examples are:

@local-table-of-contents[#:style 'immediate-only]


@subsubsection[#:tag "example_mouse_1d"]{Mouse 1D}
@(example-from-file "basics/input/mouse-1d.rkt")

@subsubsection[#:tag "example_mouse_2d"]{Mouse 2D}
@(example-from-file "basics/input/mouse-2d.rkt")

@subsubsection[#:tag "example_mouse_press"]{Mouse Press}
@(example-from-file "basics/input/mouse-press.rkt")

@subsubsection[#:tag "example_mouse_signals"]{Mouse Signals}
@(example-from-file "basics/input/mouse-signals.rkt")

@subsubsection[#:tag "example_easing"]{Easing}
@(example-from-file "basics/input/easing.rkt")

@subsubsection[#:tag "example_constrain"]{Constrain}
@(example-from-file "basics/input/constrain.rkt")

@subsubsection[#:tag "example_storing_inputs"]{Storing Input}
@(example-from-file "basics/input/storing-input.rkt")

@subsubsection[#:tag "example_mouse_functions"]{Mouse Functions}
@(example-from-file "basics/input/mouse-functions.rkt")

@subsubsection[#:tag "example_keyboard"]{Keyboard}
@(example-from-file "basics/input/keyboard.rkt")

@subsubsection[#:tag "example_keyboard_functions"]{Keyboard Functions}
@(example-from-file "basics/input/keyboard-functions.rkt")

@subsubsection[#:tag "example_milliseconds"]{Milliseconds}
@(example-from-file "basics/input/milliseconds.rkt")

@subsubsection[#:tag "example_clock"]{Clock}
@(example-from-file "basics/input/clock.rkt")



@subsection[#:tag "examples_transform"]{Transform}

The examples using transformations are:

@local-table-of-contents[#:style 'immediate-only]


@subsubsection[#:tag "example_translate"]{Translate}
@(example-from-file "basics/transform/translate.rkt")

@subsubsection[#:tag "example_scale"]{Scale}
@(example-from-file "basics/transform/scale.rkt")

@subsubsection[#:tag "example_rotate"]{Rotate}
@(example-from-file "basics/transform/rotate.rkt")

@subsubsection[#:tag "example_arm"]{Arm}
@(example-from-file "basics/transform/arm.rkt")



@subsection[#:tag "examples_typography"]{Typography}

The typography examples are:

@local-table-of-contents[#:style 'immediate-only]

@subsubsection[#:tag "example_letters"]{Letters}
@(example-from-file "basics/typography/letters.rkt")

@subsubsection[#:tag "example_words"]{Words}
@(example-from-file "basics/typography/words.rkt")

@subsubsection[#:tag "example_text_rotation"]{Text Rotation}
@(example-from-file "basics/typography/text-rotation.rkt")


@subsection[#:tag "examples_vectors"]{Vectors}

The examples using vectors are:

@local-table-of-contents[#:style 'immediate-only]

@subsubsection[#:tag "example_vectors"]{Vectors}
@(example-from-file "basics/vectors/vector.rkt")

@subsubsection[#:tag "example_vector_2d"]{Vector 2d}
@(example-from-file "basics/vectors/vector-2d.rkt")

@subsubsection[#:tag "example_vector_of_objects"]{Vector of objects}
@(example-from-file "basics/vectors/vector-of-objects.rkt")



@subsection[#:tag "examples_form"]{Form}

The form examples are:

@local-table-of-contents[#:style 'immediate-only]

@subsubsection[#:tag "example_points_and_lines"]{Points and Lines}
@(example-from-file "basics/form/point-and-lines.rkt")

@subsubsection[#:tag "example_pie_chart"]{Pie Chart}
@(example-from-file "basics/form/pie-chart.rkt")

@subsubsection[#:tag "example_regular_polygons"]{Regular Polygons}
@(example-from-file "basics/form/regular-polygons.rkt")

@subsubsection[#:tag "example_start"]{Star}
@(example-from-file "basics/form/star.rkt")

@subsubsection[#:tag "example_triangle_strip"]{Triangle Strip}
@(example-from-file "basics/form/triangle-strip.rkt")

@subsubsection[#:tag "example_bezier"]{Bezier}
@(example-from-file "basics/form/bezier.rkt")




@subsection[#:tag "examples_image"]{Image}

The image examples are:

@local-table-of-contents[#:style 'immediate-only]

@subsubsection[#:tag "example_pointilism"]{Pointilism}
@(example-from-file "basics/image/pointilism.rkt")


@;-------------------
@;-------------------
@;-------------------


@(void @~a{
           Differences between Sketching and Processing.

           - delay in P is called nap in S.
             note: nap uses milliseconds and normal sleep in R uses seconds})
           

@index-section[]


