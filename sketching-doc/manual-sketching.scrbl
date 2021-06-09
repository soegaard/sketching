#lang scribble/manual
@; raco scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-sketching.scrbl && open html/manual-sketching.html
@(require scribble/example)
@(require racket/format)
@(require (for-label ; (except-in racket/base #%app #%top)
           (except-in racket
                      #%app #%top #%module-begin class delay
                      second struct random
                      )
           sketching))

@; Used to reference other manuals.
@(define reference.scrbl '(lib "scribblings/reference/reference.scrbl"))
@(define math.scrbl      '(lib "math/scribblings/math.scrbl"))

@; Long urls

@(define (wikipedia name . preflow)
   (define url (string-append "https://en.wikipedia.org/wiki/" name))
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define (wikipedia/section url . preflow)
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define (cblas-docs name . preflow)
   (define url (string-append "https://en.wikipedia.org/wiki/" name))
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})




@title[#:tag "sketching"]{Sketching @linebreak[] A Language for Creative Coding}

@defmodule[sketching]

Sketching is a language/library for creative coding. The focus is to make
graphical programs accessible for beginners, artists, educators and designers.
Sketching is free to use and the source is available to read and improve.


The main focus of Sketching is graphical programs. Running a program
will display a canvas ready displaying static images or animation.


The inspiration for Sketching came from the Processing project. Think
of Sketching as what Processing would have looked like, if it used
Racket as its programming language instead of Java.

The documentation is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.
The documentation is based on the documentation for Processing (same documentation license).


@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]

@local-table-of-contents[]

@section{Reference}

@local-table-of-contents[#:style 'immediate-only]

@subsection{Color}

@(define draw-namespace
   (let ([ns (make-base-namespace)])
     ns
     #;(namespace-require ''racket/draw ns)))

@(define (make-sketching-eval)
   (let ([e (make-base-eval)])
     (e '(require sketching sketching/parameters racket/draw))
     ; (e '(dynamic-require 'racket/draw #f))
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

@racket[(background rgb)]            @linebreak[]
@racket[(background rgb alpha)]      @linebreak[]
@racket[(background gray)]           @linebreak[]
@racket[(background gray alpha)]     @linebreak[]
@racket[(background v1 v2 v3)]       @linebreak[]
@racket[(background v1 v2 v3 alpha)] @linebreak[]
@racket[(background image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value.")
               (list @racket[alpha] "An alpha level.")
               (list @racket[gray]  "An integer.")
               (list @racket[v1]    "Red or hue value.")
               (list @racket[v2]    "Green or saturation value.")
               (list @racket[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racket[v1], @racket[v2], @racket[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racket[v1], @racket[v2], @racket[v3] are hsb-values.


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

@racket[(red rgb)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(green rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(blue rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


@bold{Description}

Extracts the blue value from a color.

The blue component is a value from 0 to the maximum blue level.
A blue value of 0 means no blue and the maximum means fully blue.
The default range of blue values is 0 to 255. Use @racket[color-mode]
to change the default range.

@;---------


@subsubsection{alpha}

@bold{Name: } @defidentifier[#'alpha]

Extracts the alpha component of the color @racket[c].

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

@racket[(alpha rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(hue rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(saturation rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(brightness rgb)] @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value."))]


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

@racket[(fill rgb)]            @linebreak[]
@racket[(fill rgb alpha)]      @linebreak[]
@racket[(fill gray)]           @linebreak[]
@racket[(fill gray alpha)]     @linebreak[]
@racket[(fill v1 v2 v3)]       @linebreak[]
@racket[(fill v1 v2 v3 alpha)] @linebreak[]
@racket[(fill image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value.")
               (list @racket[alpha] "An alpha level.")
               (list @racket[gray]  "An integer.")
               (list @racket[v1]    "Red or hue value.")
               (list @racket[v2]    "Green or saturation value.")
               (list @racket[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racket[v1], @racket[v2], @racket[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racket[v1], @racket[v2], @racket[v3] are hsb-values.


@bold{Description}

Sets the color used to fill shapes. For example, if you run @racket[(fill 204 102 0)],
all subsequent shapes will be filled with orange. This color
is either specified in terms of the RGB or HSB color depending on the
current @racket[color-Mode]. The default color space is RGB, with each value
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

@racket[(stroke rgb)]            @linebreak[]
@racket[(stroke rgb alpha)]      @linebreak[]
@racket[(stroke gray)]           @linebreak[]
@racket[(stroke gray alpha)]     @linebreak[]
@racket[(stroke v1 v2 v3)]       @linebreak[]
@racket[(stroke v1 v2 v3 alpha)] @linebreak[]
@racket[(stroke image)]          @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[rgb]   "A color value.")
               (list @racket[alpha] "An alpha level.")
               (list @racket[gray]  "An integer.")
               (list @racket[v1]    "Red or hue value.")
               (list @racket[v2]    "Green or saturation value.")
               (list @racket[v3]    "Blue or brightness value."))]

If the color mode is rgb, then the values @racket[v1], @racket[v2], @racket[v3] are rgb-values. @linebreak[]
If the color mode is hsb, then the values @racket[v1], @racket[v2], @racket[v3] are hsb-values.


@bold{Description}


Sets the color used to draw lines and borders around shapes.
For example, after @racket[(stroke 204 102 0)], all subsequent shapes will be filled with orange.
This color is either specified in terms of the RGB or HSB color depending on the
current @racket[color-Mode]. The default color space is RGB, with each value
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

@racket[(no-fill)]            @linebreak[]


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

@racket[(no-stroke)]            @linebreak[]


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

@racket[(color-mode mode)]                         @linebreak[]
@racket[(color-mode mode max)]                     @linebreak[]
@racket[(color-mode mode max1 max2 max3)]          @linebreak[]
@racket[(color-mode mode max1 max2 max3 maxA)]     @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[mode]  "'rgb or 'hsb corresponding to red/greeb/blue or huse/saturation/brightness")
               (list @racket[max]   "maximum value for all color elements")
               (list @racket[max1]  "maximum value for the red value or hue value according to the mode")
               (list @racket[max2]  "maximum value for the green value or saturation value according to the mode")
               (list @racket[max3]  "maximum value for the blue value or brightness value according to the mode")
               (list @racket[maxA]  "maximum value for the alpha value"))]


@bold{Description}

Changes the way Sketching interprets color data. By default, the
parameters for @racket[fill], @racket[stroke], @racket[background], and @racket[color] are defined
by values between 0 and 255 using the RGB color model. The @racket[color-mode]
function is used to change the numerical range used for specifying
colors and to switch color systems. For example, calling
@racket[(colorMode 'rgb 1.0)] will specify that values are specified between 0
and 1. The limits for defining colors are altered by setting the
parameters @racket[max], @racket[max1], @racket[max2], @racket[max3], and @racket[maxA].

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

@racket[(lerp-color color1 color22 amount)]                         @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[color1]  "interpolate from this color")
               (list @racket[color2]  "interpolate to this color")
               (list @racket[amount]  "value between 0.0 and 1.0"))]


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
          (arc 50 55 50 50 0 half-pi)
          (no-fill)
          (arc 50 55 60 60 half-pi    pi)
          (arc 50 55 70 70 pi      (+ pi quarter-pi))
          (arc 50 55 80 80 (+ pi quarter-pi) two-pi)
          (eval:alts        (arc 50 55 80 80 (+ pi quarter-pi) two-pi)
                     (begin (arc 50 55 80 80 (+ pi quarter-pi) two-pi)
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

@racket[(arc a b c d start stop)]                         @linebreak[]
@racket[(arc a b c d start stop mode)]                    @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[a]       "x-coordinate of the arc's ellipse")
               (list @racket[b]       "y-coordinate of the arc's ellipse")
               (list @racket[c]       "width of the arc's ellipse by default")
               (list @racket[d]       "height of the arc's ellipse by default")
               (list @racket[start]   "angle to start the arc, specified in radians")
               (list @racket[stop]    "angle to stop the arc, specified in radians")
               (list @racket[mode]    "one of: 'open-pie 'pie 'open 'chord"))]


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

@racket[(circle x y extent)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[x]       "x-coordinate of the arc's ellipse")
               (list @racket[y]       "y-coordinate of the arc's ellipse")
               (list @racket[extent]  "width of the arc's ellipse by default"))]


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

@racket[(ellipse a b c d)]            @linebreak[]

@bold{Arguments}

The default interpretation of the arguments is:

@tabular[#:sep @hspace[1]
         (list (list @racket[a]   "x-coordinate of the arc's ellipse")
               (list @racket[b]   "y-coordinate of the arc's ellipse")
               (list @racket[c]   "width of the arc's ellipse by default")
               (list @racket[d]   "height of the arc's ellipse by default"))]

The interpretation of the arguments are affected by @racket[ellipse-mode].


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

@racket[(line x1 y1 x2 y2)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[x1]   "x-coordinate of the first point")
               (list @racket[y1]   "y-coordinate of the first point")
               (list @racket[x2]   "x-coordinate of the second point")
               (list @racket[y2]   "y-coordinate of the second point"))]



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

@racket[(point x y)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[x]   "x-coordinate of the point")
               (list @racket[y]   "y-coordinate of the point"))]



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

@racket[(quad x1 y1  x2 y2  x3 y3  x4 y4)]            @linebreak[]

@bold{Arguments}

@tabular[#:sep @hspace[1]
         (list (list @racket[x1]   "x-coordinate of the first corner")
               (list @racket[y1]   "y-coordinate of the first corner")
               (list @racket[x2]   "x-coordinate of the second corner")
               (list @racket[y2]   "y-coordinate of the second corner")
               (list @racket[x3]   "x-coordinate of the third corner")
               (list @racket[y3]   "y-coordinate of the third corner")
               (list @racket[x4]   "x-coordinate of the fourth corner")
               (list @racket[y5]   "y-coordinate of the fourth corner"))]


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

@racket[(rect a b c d)]              @linebreak[]
@racket[(rect a b c d r)]            @linebreak[]
@;@racket[(rect a b c d tl tr br bl)]  @linebreak[] TODO

@bold{Arguments}


The default interpretation of the arguments is:


@tabular[#:sep @hspace[1]
         (list (list @racket[a]    "x-coordinate of corner")
               (list @racket[b]    "y-coordinate of corner")
               (list @racket[c]    "width")
               (list @racket[d]    "height")
               (list @racket[r]    "radii for all four corners")
               #;(list @racket[tl]   "radius for top-left corner")
               #;(list @racket[tr]   "radius for top-right corner")
               #;(list @racket[br]   "radius for bottom-right corner")
               #;(list @racket[bl]   "radius for bottom-left corner"))]

The interpretation of the arguments are affected by @racket[rect-mode].


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

@racket[(square x y extent)]              @linebreak[]

@bold{Arguments}


The default interpretation of the arguments is:


@tabular[#:sep @hspace[1]
         (list (list @racket[x]      "x-coordinate of corner")
               (list @racket[y]      "y-coordinate of corner")
               (list @racket[extent] "length of side"))]

The interpretation of the arguments are affected by @racket[rect-mode].


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

@racket[(triangle x1 y1  x2 y2  x3 y3)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[x1]   "x-coordinate of the first corner")
               (list @racket[y1]   "y-coordinate of the first corner")
               (list @racket[x2]   "x-coordinate of the second corner")
               (list @racket[y2]   "y-coordinate of the second corner")
               (list @racket[x3]   "x-coordinate of the third corner")
               (list @racket[y3]   "y-coordinate of the third corner"))]

@bold{Description}

A triangle is a shape created by connecting three points. The first
two arguments specify the first point, the middle two arguments
specify the second point, and the last two arguments specify the third
point.


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

@racket[(ellipse-mode mode)]        @linebreak[]

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

@racket[(rect-mode mode)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[mode] "one of: 'center 'radius 'corner 'corners"))]

@bold{Description}

Modifies the location from which rectangles and squares are drawn by changing the
way in which arguments given to @racket[rect] and @racket[square] are intepreted.

The default mode is @racket[(rect-mode 'corner)], which interprets the first two
arguments of rect() as the upper-left corner of the shape, while the
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

@racket[(stroke-cap cap)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[cap] "one of: 'round 'square 'project"))]

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

@racket[(stroke-join join)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[join] "one of: 'miter 'bevel 'round"))]

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

@racket[(stroke-weight weight)]        @linebreak[]

@bold{Arguments}


@tabular[#:sep @hspace[1]
         (list (list @racket[weight] "the weight (in pixels) of the stroke"))]

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


@bold{Usage}

@racket[(begin-shape)]        @linebreak[]

@;bold{Arguments}
@;tabular[#:sep @hspace[1] (list (list @racket[weight] "the weight (in pixels) of the stroke"))]

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

@racket[(end-shape)]          @linebreak[]
@racket[(end-shape mode)]     @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racket[mode] @racket['close]))]


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

@racket[(vertex x y)]          @linebreak[]

@bold{Arguments}
@tabular[#:sep @hspace[1]
         (list (list @racket[x] "x-coordinate of the point")
               (list @racket[y] "y-coordinate of the point"))]


@bold{Description}

Add the point (x,y) to the current shape.
The @racket[vertex] functions must be called between @racket[begin-shape]
and @racket[end-shape].


@index-section[]
