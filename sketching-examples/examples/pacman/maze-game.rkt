#lang sketching
;;;
;;; UNNAMED MAZE GAME
;;;

; Inspiration:
; - https://en.wikipedia.org/wiki/Category:Maze_games
; - For Pacman related information: "The Pacman Dosier".
; - Games: Pacman, Ladybug, Bomberman

; TODO
; - improve attract screen
; - draw-text needs more colors
; - faster initial speed - or an easy, normal, hard option?

(require racket/set racket/list racket/match racket/port
         (only-in racket/gui bitmap% bitmap-dc%)
         rsound)


;;; GAME PLAY

; - maze game in the pacman tradition
; - enemies follow the hero
; - power ups
; - portals?
; - shooting?
; - revolving doors?


;;; VISUAL

; - retro 8-bit look
; - limited color palette 
; - screen divided into 8x8 tiles
; - draw everything in low resolution -> then scale 

;;;
;;; Frame Rate
;;;

(define FRAME-RATE 40)

;;;
;;; SCREEN
;;;

; The screen consists of ROWSxCOLS tiles.
; Each tile is 8x8 pixels.

(define TOP-ROWS    3)                  ; rows for status (high score etc)
(define ROWS       26)                  ; rows for the maze
(define TOTAL-ROWS (+ TOP-ROWS ROWS))   ; total number of rows

(define COLS     28)                    ; columns

; Each tile has size 8x8. The resolution thus becomes:
(define RESOLUTION (list (* 8 TOTAL-ROWS) (* 8 COLS)))
; which is 232 x 224.

; We base everything on this low resolution and simply scale everything up.
; This results in a retro pixel look.

(define SCALE         3)
(define SCREEN-WIDTH  (* 8 COLS))       ; low resolution
(define SCREEN-HEIGHT (* 8 TOTAL-ROWS)) ; low resolution

(define MAZE-WIDTH  (* 8 COLS)) ; low resolution
(define MAZE-HEIGHT (* 8 ROWS)) ; low resolution

(define ACTUAL-SCREEN-WIDTH  (* SCALE SCREEN-WIDTH))
(define ACTUAL-SCREEN-HEIGHT (* SCALE SCREEN-HEIGHT))

; Each frame will be rendered to the screen bitmap (with low resolution).
; Then the screen bitmap drawn and scaled to the actual display.
(define screen-bm (make-object bitmap% SCREEN-WIDTH SCREEN-HEIGHT))
(define screen-dc (new bitmap-dc% [bitmap screen-bm]))

; The maze is static, so we only render it when a new level starts
(define maze-bm (make-object bitmap% MAZE-WIDTH MAZE-HEIGHT))
(define maze-dc (new bitmap-dc% [bitmap maze-bm]))


;;;
;;; Sounds
;;;

(define mute-mode #t)

(define dot-eaten.wav (rs-read "dot-eaten.wav"))

(define pstream (make-pstream #:buffer-time (/ 100 1000.)))

(define (play-sound name)
  (unless mute-mode
    (case name
      [(dot-eaten)
       (pstream-play  pstream dot-eaten.wav)])))

;;;
;;; Tiles
;;;

; The graphics used for tiles are stored in tile sheets.
; Each tile sheet is a bitmap (png).
; The tile sheet used for the maze consists of:
;   Row 0-2: Maze elements used for the outer parts of the maze (double lined).
;   Row 3-5: Maze elements used for the inner parts of the maze (single lined).
;   Row 6:   dots and energizer pill

; We use a tilesheet structure to keep information on the tiles.
; When using images from the web as tile sheets, you'll quick find
; out that some tilesheets doesn't start at (0,0) and that some
; tilesheets have lines (gaps) between each tile. The tilesheet
; structure is prepared for such cases.

(struct tilesheet (bitmap
                   left upper        ; (x,y) of the corner where the sheet begins 
                   rows cols         ; number of rows and columns in the sheet
                   hor-gap ver-gap   ; gaps between columns and rows respectively
                   width height))    ; width and height of each time


(define (load-tilesheet path width height rows cols
                        #:left    [left    0]
                        #:upper   [upper   0]
                        #:hor-gap [hor-gap 0]
                        #:ver-gap [ver-gap 0])
  (define bitmap (load-image path))
  (tilesheet bitmap left upper rows cols hor-gap ver-gap width height))

; Let's load our maze tilesheet. 

(define maze-sheet (load-tilesheet "maze-tilesheet.png"
                                   8 8 
                                   6 12))

; Each tile has an id number.
; For a tile sheet with 3 rows and 4 columns, the id numbers are:
;    0  1  2  3
;    4  5  6  7
;    8  9 10 11

; We need some helpers to compute row and column number given the tile id.

(define (tile-id->row ts id)
  (quotient  id (tilesheet-cols ts)))

(define (tile-id->col ts id)
  (remainder id (tilesheet-cols ts)))



; Using these helpers we can now compute the (x,y)-position of a given tile on the sheet.

(define (tile-id->sheet-x ts id)
  (define j (tile-id->col ts id))
  (+ ts.left (* j (+ ts.width ts.hor-gap))))

(define (tile-id->sheet-y ts id)
  (define i (tile-id->row ts id))
  (+ ts.upper (* i (+ ts.height ts.ver-gap))))

; We are now able to draw a tile on the screen.

(define (draw-tile ts id x y)
  ; draw the tile on tile sheet `ts` with id `id` at position (x,y) on the screen
  (define sheet-x (tile-id->sheet-x ts id))
  (define sheet-y (tile-id->sheet-y ts id))
  (image ts.bitmap
         x y
         ts.width ts.height   ; destination
         sheet-x sheet-y
         ts.width ts.height)) ; source

; While debugging it is convenient to display a given tile in the repl.
; If we draw the tile to a bitmap instead of the screen, we can
; see it directly in the DrRacket or Emacs repl.
; Try  (tile-id->bitmap maze-sheet 0)  to see the first tile.

(define (tile-id->bitmap ts id)
  (define bm (make-object bitmap% (* 4 ts.width) (* 4 ts.height)))
  (define dc (new bitmap-dc% [bitmap bm]))
  (with-dc dc
    (send dc set-scale 4 4)
    (draw-tile ts id 0 0))
  bm)

; Looping over all rows and columns we can draw the entire maze.
; How the level data and the translation from characters to tile ids
; work is shown later on.

(define (draw-tiles ts)
  (for ([i (in-range ROWS)])
    (for ([j (in-range COLS)])
      (define id (char->tile-id level-data_i_j i j))
      ; (define id tiles_i_j)
      (draw-tile ts id (* j ts.width) (* i ts.height)))))

; For debugging it is extremely convenient to have an image of all tiles
; with their tile ids. Try (tilesheet-with-numbers maze-sheet) in he repl.

; (tilesheet-with-numbers maze-sheet)

(define (tilesheet-with-numbers ts)
  ; Let's make an image that shows the id number of the individual tiles.
  (define s 8) ; scale (output)
  (define g 1) ; gap   (output)
  (define bm (make-object bitmap%
                          (* ts.cols (+ (* s ts.width)  g))
                          (* ts.rows (+ (* s ts.height) g))))
  (define bm-dc (new bitmap-dc% [bitmap bm]))
  (with-dc bm-dc
    (smoothing 'unsmoothed)
    (image-mode 'corner)
    (background "yellow")
    (fill "white") ; text color
    (text-size 1.4)
    (scale s)
    (rect-mode 'corner)
    (define id 0)
    (for ([i (in-range ts.rows)])
      (for ([j (in-range ts.cols)])
        (define x (* j (+ ts.width  (/ g s))))
        (define y (* i (+ ts.height (/ g s))))
        (draw-tile ts id x y)
        (text (~a id) x y)
        (set! id (+ id 1)))))
  bm)

;;;
;;; LEVELS
;;;

; The levels are drawn with unicode.

(define level-data
  (vector
   ;0   4   8   2   6   0   4   8
   "╔════════════╕╒════════════╗"
   "║............║║............║"
   "║.┌──┐.┌───┐.║║.┌───┐.┌──┐.║"
   "║*│  │.│   │.║║.│   │.│  │*║"
   "║.└──┘.└───┘.╚╝.└───┘.└──┘.║"
   "║..........................║"
   "║.┌──┐.┌┐.┌──────┐.┌┐.┌──┐.║"
   "║.└──┘.││.└──┐┌──┘.││.└──┘.║"
   "║......││....││....││......║"
   "╚════╗.│└──┐ └┘ ┌──┘│.╔════╝"
   "     ║.│┌──┘    └──┐│.║     "
   "     ║.││          ││.║     "
   "     ║.││ ╔______╗ ││.║     "
   "═════╝.└┘ ║      ║ └┘.╚═════"
   "Ttttt .   ╚══════╝   .tttttT"
   "═════╗.┌┐ ┌──────┐ ┌┐.╔═════"
   "     ║.││ └──┐┌──┘ ││.║     "
   "╔════╝.└┘    ││    └┘.╚════╗"
   "║............││............║"
   "║.┌──┐.┌───┐.└┘.┌───┐.┌──┐.║"
   "║.└─┐│.│   │....│   │.│┌─┘.║"
   "║*..││.│   └────┘   │.││..*║"
   "╚═╗.││.│            │.││.╔═╝"
   "╔═╝.└┘.└────────────┘.└┘.╚═╗"
   "║..........................║"
   "╚══════════════════════════╝"))

; T = tunnel portal (connects to other side)
; t = tunnel (slow down actors)
; * = energizer pellet
; . = dot
; _ = ghost door

; There is a slight problem going from a character in the above
; unicode drawing to a tile from the tile sheet.
; The tile sheet has two vertical walls. One where the wall is
; slightly to the left of the center and one where it is to the right.
; This means we need to figure out if a | in the drawing is a
; left or right wall.
; Similarly there is upper and lower horizontal walls.

; Scanning a line from left to right, we can keep track of
; whether the number of vertical walls seen so far is odd or even.

; Finally there are two types of corners.
; Consider:  left wall,  ╔ , upper wall
; versus     right wall, ╔ , lower wall
; The first case needs a larger corner. The second case needs a small corner. 

(define (left?  s) (set-member? s 'left))
(define (right? s) (set-member? s 'right))
(define (upper? s) (set-member? s 'upper))
(define (lower? s) (set-member? s 'lower))



(define (annotate-level level)
  (define ht (make-hash)) ; (list x y) -> set of values

  (define (get i j)
    (hash-ref ht (list i j) (set)))
  (define (add! i j val)
    (define key (list i j))
    (define old (hash-ref ht key (set)))
    (hash-set! ht key (set-add old val)))
  (define (remove! i j val)
    (define key (list i j))
    (define old (hash-ref ht key (set)))
    (hash-set! ht key (set-remove old val)))
  (define (left!  i j) (remove! i j 'right) (add! i j 'left))
  (define (right! i j) (remove! i j 'left)  (add! i j 'right))
  (define (upper! i j) (remove! i j 'lower) (add! i j 'upper))
  (define (lower! i j) (remove! i j 'upper) (add! i j 'lower))
    
  (define doubles 0)
  (define singles 0)
  (define (reset!) (set! doubles 0) (set! singles 0))
  (define parity 0)
  ; 1. Categorize corners  left/right
  (reset!)
  (for ([i (in-range ROWS)])
    ; Scan each line horisontally
    (for ([j (in-range COLS)])
      (define c level-data_i_j)
      (case c
        [(#\╔ #\╗ #\╕ #\╒ #\╚ #\╝ #\║) ; doubles  #\╚ #\╝
         (define side (if (even? doubles) 'left 'right))
         (add! i j side)         
         (set! doubles (+ doubles 1))]
        [(#\┌ #\┐ #\└ #\┘ #\│ #\|)         ; singles  #\└ #\┘
         (define side (if (even? singles) 'right 'left))
         (add! i j side)
         (set! singles (+ singles 1))
         (case c
           ; if there is a ver wall to the left/right, singles stay the same
           [(#\┌ #\└ #\┘ #\┐) 
            (define k (- j 1))
            (define l (+ j 1))
            (case level-data_i_k
              [(#\│ #\|) (set! singles (- singles 1))])
            (case level-data_i_l
              [(#\│ #\|) (set! singles (- singles 1))])])]
        [(#\T) ; tunnel portal
         (set! doubles (+ doubles 1))]
        #;[(#\─ #\═)
         ; these counts as starters, if they are first on a line,
         ; or if they have a space to the left
         (define k (- j 1))
         (define side (or (= j 0) ; first on line
                          (eqv? level-data_i_k #\space)))
         (when side
           ; (add! i j side)
           (case c
             [(#\─) (set! singles (+ singles 1))]
             [(#\═) (set! doubles (+ doubles 1))]))]))
    (reset!))
  ; 2. Categorize corners  upper/lower
  (reset!)
  (for ([j (in-range COLS)])
    ; Scan each column 
    (for ([i (in-range ROWS)])
      (define c level-data_i_j)
      (case c
        [(#\╔ #\╗  #\╚ #\╝ #\═ #;#\_) ; #\╒ #\╕
         (define placement (if (even? doubles) 'upper 'lower))
         (add! i j placement)
         (set! doubles (+ doubles 1))]
        [(#\┌ #\┐ #\└ #\┘ #\─ #\-)  
         (define placement (if (even? singles) 'lower 'upper))
         (add! i j placement)
         (set! singles (+ singles 1))
         (case c ; if there is a hor wall on above or below, singles stay the same
           [(#\┌ #\┐ #\└ #\┘) 
            (define k (- i 1))
            (define l (+ i 1))
            (when (or (member level-data_k_j '(#\─ #\-))
                      #;(member level-data_l_j '(#\─ #\-)))
              (set! singles (- singles 1)))])]))
    (reset!))
  ; 3.fix left/right and upper/lower for corner pieces
  ;   Fix horizontal too.
  (for ([i (in-range ROWS)])
    ; Scan each line horizontally
    (for ([j (in-range COLS)])
      (define c level-data_i_j)
      (case c        
        [(#\╔ #\╗ #\┌ #\┐) ; connects down
         (if (left? (get (+ i 1) j)) (left! i j) (right! i j))]
        [(#\╚ #\╝ #\└ #\┘) ; connects up
         (if (left? (get (- i 1) j)) (left! i j) (right! i j))])
      (case c        
        [(#\╔ #\╚ #\└ #\┌) ; connects right 
         (if (upper? (get i (+ j 1))) (upper! i j) (lower! i j))]
        [(#\╗ #\╝ #\┘ #\┐) ; connects left
         (if (upper? (get i (- j 1))) (upper! i j) (lower! i j))])
      (case c        
        [(#\═ #\─ #\-) ; connects both left and right 
         (when (and (upper? (get i (- j 1))) (get i (+ j 1)))
           (upper! i j))
         (when (and (lower? (get i (- j 1))) (get i (+ j 1)))
           (lower! i j))]))
    (reset!))
  ; 4. Find dots, pellets, tunnels etc
  (define dots-in-level-count 0)  
  (for ([i (in-range ROWS)])
    (for ([j (in-range COLS)])
      (define c level-data_i_j)
      (case c
        [(#\*) (add! i j 'pellet)]        
        [(#\t) (add! i j 'tunnel)]
        [(#\T) (add! i j 'portal)]
        [(#\.) (add! i j 'dot)
               (+= dots-in-level-count 1)])))
  (values ht dots-in-level-count))


(define-values (level-annotations dots-in-level-count)
  (annotate-level level-data))

(define (convert-datum-to-python value)
  (define (atom? v)
    (or (number? v) (boolean? v) (symbol? v) (string? v)))
  (define (convert v)
    (cond
      ; atoms
      [(atom? v)  v]
      ; compound values are converted to tagged trees
      [(set? v)    (cons 'set   (for/list ([x v])     (convert x)))]
      [(list? v)   (cons 'tuple (for/list ([x v])     (convert x)))]
      ; [(vector? v) (cons 'array (for/list ([x v])     (convert x)))]
      [(hash? v)   (cons 'dict  (for/list ([(k x) v]) (list (convert k) (convert x))))]
      [else
       (raise-arguments-error 'convert "unexpected data")]))
  (define (emit t)
    (match t
      ; atoms
      [#t                          (display "True")]
      [#f                          (display "False")]      
      [(? number? t)               (display t)]
      [(? string? t)               (display t)]
      [(? symbol? t)               (display (~a "'" t "'"))] ; 
      ; compound values
      [(list  'set)                (display "set()")]
      [(list  'set x)              (display "set([") (emit x) (display "])")]
      [(list* 'set x xs)           (display "set([") (emit x)
                                   (for ([x xs]) (display ",") (emit x))
                                   (display "])")]
      [(list  'tuple)              (display "()")]
      [(list  'tuple x)            (display "(") (emit x) (display ")")]
      [(list* 'tuple x xs)         (display "(") (emit x)
                                   (for ([x xs]) (display ",") (emit x))
                                   (display ")")]
      [(list  'array)              (display "[]")]
      [(list  'array x)            (display "[") (emit x) (display "]")]
      [(list* 'array x xs)         (display "[") (emit x)
                                   (for ([x xs]) (display ",") (emit x))
                                   (display "]")]
      [(list  'dict)               (display "{}")]
      [(list  'dict (list k v))    (display "{")
                                   (emit k) (display ":") (emit v)
                                   (display "}")]
      [(list* 'dict kv kvs)        (display "{")
                                   (define-values (k v) (values (car kv) (cadr kv)))
                                   (emit k) (display ":") (emit v)
                                   (for ([kv kvs])
                                     (define-values (k v) (values (car kv) (cadr kv)))
                                     (display ",")
                                     (emit k) (display ":") (emit v))
                                   (display "}")]
      [_ "error: unexpected data"]))
  (with-output-to-string
    (λ () (emit (convert value)))))
  
  

(define (reset-level-data!)
  (set!-values (level-annotations dots-in-level-count)
               (annotate-level level-data)))

(define (get-annotation i j)
  (hash-ref level-annotations (list i j) (set)))

(define (remove-annotation! i j val)
  (define a (hash-ref level-annotations (list i j) (set)))
  (hash-set! level-annotations (list i j) (set-remove a val)))

(define (ann id)
  (define-values (i j) (id->ij id))
  (get-annotation i j))

(define (remove-ann id val)
  (define-values (i j) (id->ij id))
  (remove-annotation! i j val))

(define (accessible? id)
  ; is `id` a cell that the hero can enter?
  (define a (ann id))
  (and (not (set-member? a 'left))
       (not (set-member? a 'right))
       (not (set-member? a 'lower))
       (not (set-member? a 'upper))))

(define (~annotation a)
  (~a (cond [(set-member? a 'upper) 'U]
            [(set-member? a 'lower) 'L]
            [else                  " "])
      (cond [(set-member? a 'left)  'L]
            [(set-member? a 'right) 'R]
            [else                  " "])))

(define (draw-coordinates ts)
  (define s SCALE) ; scale
  (define g 0)     ; gap
  (text-size 2)
  (rect-mode 'corner)
  (for ([i (in-range ROWS)])
    (for ([j (in-range COLS)])
      (define x (* j              (+ ts.width  (/ g s))))
      (define y (* (+ i TOP-ROWS) (+ ts.height (/ g s))))
      (when (or (= j 0) #;(zero? (remainder i 4)))
        (fill "yellow")
        (text (~a i ) x y))
      (when (or (= i 0) #;(zero? (remainder j 4)))
        (fill "white")
        (text (~a " " j) x y))))
  ; grid
  (for ([i (in-range 0 ROWS 4)])
    (define y     (*      (+ ts.height (/ g s))))
    (define xmax  (* COLS (+ ts.width  (/ g s))))
    (define ymax  (* ROWS (+ ts.height (/ g s))))    
    (stroke "gray")
    (line 0 y xmax y))
  (for ([j (in-range 0 COLS 4)])
    (define x     (* j    (+ ts.width  (/ g s))))
    (define xmax  (* COLS (+ ts.width  (/ g s))))
    (define ymax  (* ROWS (+ ts.height (/ g s))))    
    (stroke "gray")
    (line x 0 x ymax))
  ; Annotations
  (for ([i (in-range ROWS)])
    (for ([j (in-range COLS)])
      (define x (* j              (+ ts.width  (/ g s))))
      (define y (* (+ i TOP-ROWS) (+ ts.height (/ g s))))
      (fill "yellow")
      (text (~annotation (get-annotation i j))
            (+ x (* 1/4 ts.width))
            (+ y (* 1/4 ts.height))))))


(define (char->tile-id c i j)
  (define props (hash-ref level-annotations (list i j) (set)))
  (define ul (set 'upper 'left))
  (define ur (set 'upper 'right))
  (define ll (set 'lower 'left))
  (define lr (set 'lower 'right))
  (define l  (set 'left))
  (define r  (set 'right))
  (define up  (set 'upper))
  (define lo  (set 'lower))
  (define id
    (case c
      [(#\╔ #\┌)
       (cond
         [(set=? props ul)  0]
         [(set=? props lr)  3]
         [(set=? props ur)  6]
         [(set=? props ll)  9]
         [else              3])]
      [(#\╗ #\┐)
       (cond
         [(set=? props ur)  2]
         [(set=? props ll)  5]
         [(set=? props ul)  8]
         [(set=? props lr) 11]
         [else              5])]
      [(#\╚ #\└)
       (cond
         [(set=? props ll) 24]
         [(set=? props ur) 27]
         [(set=? props lr) 30]
         [(set=? props ul) 33]
         [else             27])]
      [(#\╝ #\┘)
       (cond
         [(set=? props lr) 26]
         [(set=? props ul) 29]
         [(set=? props ll) 32]
         [(set=? props ur) 35]
         [else             29])]

      [(#\║ #\| #\│)
       (cond
         [(left?  props) 12]
         [(right? props) 14]
         [else           12])]
      
      [(#\═ #\= #\─)
       (cond
         [(upper? props)  1]
         [(lower? props) 25]
         [else            1])]
      
      [(#\_)     13] ; TODO ghost door
      [(#\╕)     72] 
      [(#\╒)     73] 
      [(#\space) 13] 
      [(#\.)     144] ; small yellow dot
      [(#\*)     145] ; large red dot
      [else      #f]))
  (case c
    [(#\┐ #\┌ #\┘ #\└ #\| #\│ #\─)
     (when id
       ; There are 36 tiles in each set,
       ; so we can change from doubles to singles
       ; by adding 36.
       (set! id (+ id 36)))])
  (or id 13))


;; ; ╔ top, left
;; ; ╗ top, right
;; ; ╚ bottom, left
;; ; ╝ bottom, right
;; ; ║ vertical wall
;; ; ╦ hor wall, connects down
;; ; ╩ hor wall, connects up
;; ; ╠ ver wall, connects right
;; ; ╣ ver wall, connects left

;;;
;;; CHARACTER TILESHEET
;;;

; The font is stored as a png of depth 32.
; The bitmap is actually a monochrome image, so we use 'png/mask
; to load the image, and then extracts the mask afterwards to get
; a depth 1 monochrome image. Such monochrome images can be used
; as masks in draw-bitmap-section - which means we can drae the
; characters with different colors.

(define (load-font in)
  (define bm   (make-object bitmap% in 'png/mask))
  (define mask (send bm get-loaded-mask))
  mask)

(define char-tilesheet-bitmap (load-font "font.png"))
; From the C64 game "The Great Giana Sisters".
; To change the font, open the project file in VChar64, export to .bin.
; Run font.rkt to convert the binary font.bin to a png file.

(define char-width  8)
(define char-height 8)

(define chars
  (list
   " ABCDEFGHIJKLMNOPQRSTUVWXYZØ    "
   "              +-0123456789      "))

(define (compute-char-positions)
  (define ht (make-hash))
  (for ([row chars] [i (in-naturals)])
    (for ([c row] [j (in-naturals)])
      (hash-set! ht c (list (* char-width j) (* char-height i)))))
  ht)

(define char-positions (compute-char-positions))

(define (draw-char c x y color)
  (define pos (hash-ref char-positions c (list #f #f)))
  (define sheet-x (first pos))
  (define sheet-y (second pos))
  (when sheet-x
    (image-mask char-tilesheet-bitmap 
                x y
                color
                char-width char-height  ; destination
                sheet-x sheet-y
                char-width char-height)))

(define (draw-text t x y color)
  (for ([c (in-string t)]
        [i (in-naturals)])
    (draw-char c (+ x (* char-width i)) y color)))


;;;
;;; MAZE CELL IDS
;;;

; The maze can be thought of as ROWSxCOLS cells.
; Each time get an id number.

;             0      1      2 ...          COLS-1
;          COLS COLS+1 COLS+2 ...        2*COLS-1
;        2*COLS ...           ...        3*COLS-1
;         ...                              ...
; (ROWS-1)*COLS ...           ... (ROWS-1)*COLS-1

(define LAST-ROW-START (* (- ROWS 1) COLS))
(define LAST-ROW-END   (- (* (- ROWS 1) COLS) 1))

(define (ij->id i j) (+ (* i COLS) j))
(define (id->ij id)  (quotient/remainder id COLS))
(define (id-delta id1 id2)
  (define-values (i1 j1) (id->ij id1))
  (define-values (i2 j2) (id->ij id2))
  (values (- i2 i1) (- j2 j1)))
(define (id-add-ij id Δi Δj)
  (define-values (i j) (id->ij id))
  (ij->id (+ i Δi) (+ j Δj)))  

(define (first-col? id) (and (integer? id) (zero? (remainder id COLS))))
(define (last-col? id)  (and (integer? id) (zero? (remainder (+ id 1) COLS))))
(define (first-row? id) (and (integer? id) (< -1 id COLS)))
(define (last-row?  id) (and (integer? id) (<= LAST-ROW-START id LAST-ROW-END)))

(define (portal? id) (set-member? (ann id) 'portal))
(define (tunnel? id) (set-member? (ann id) 'tunnel))

; Given a cell id, we'll need a way to compute the ids of the neighbour cells.

;        up
;  left  id  right
;       down

(define (up    id) (if (first-row? id) #f (- id COLS)))
(define (down  id) (if (last-row?  id) #f (+ id COLS)))
(define (left  id) (if (first-col? id) (if (portal? id) (+ id COLS -1) #f) (- id 1)))
(define (right id) (if (last-col?  id) (if (portal? id) (- id COLS -1) #f) (+ id 1)))




;;;
;;; SPRITE TILESHEET
;;;

;(define sprite-tilesheet-bitmap (load-image "sprites.png"))
(define sprite-tilesheet-bitmap (load-image "sprites2.png"))

(define sprite-sheet (load-tilesheet "sprites2.png"
                                     16 16           ; width x height (in pixels)
                                     12 12))         ; rows  x cols   (in number of tiles)

; Note: Even though the sprite sheet uses 16x16 it is possible
;       for some sprites to be smaller.

(define (draw-sprite id x y)
  (define sheet sprite-sheet)
  ; draw the tile with id `id` at position (x,y) on the screen
  (define sheet-x (tile-id->sheet-x sprite-sheet id))
  (define sheet-y (tile-id->sheet-y sprite-sheet id))
  (image sheet.bitmap
         x y
         sheet.width sheet.height   ; destination
         sheet-x sheet-y
         sheet.width sheet.height)) ; source

(define (sprite-id->bitmap id)
  (define sheet sprite-sheet)  
  (define bm (make-object bitmap% (* 4 sheet.width) (* 4 sheet.height)))
  (define dc (new bitmap-dc% [bitmap bm]))
  (with-dc
    (send dc set-scale 4 4)
    (draw-sprite id 0 0))
  bm)

;;;
;;; ANIMATED SPRITES
;;;

(struct animated-sprite
  (sheet
   frames  ; vector of sprite ids, one for each animation tile
   times   ; how many frames to display each frame
   current ; index into frames
   left))  ; how many frames left before current must be update


(define (draw-animated-sprite s x y)
  (define c s.current)
  (draw-sprite s.frames_c x y))

(define (update-animated-sprite s)
  (define n (vector-length s.frames))
  (case s.left
    [(0)  (:= s.current (modulo (+ s.current 1) n))
          (:= s.left (vector-ref s.times s.current))]
    [else (:= s.left (- s.left 1))]))


(define hero-right-animation (animated-sprite sprite-sheet #( 0  1  2  3  2  1) #(0 0 0 0 0 0) 0 1))
(define hero-left-animation  (animated-sprite sprite-sheet #(12 13 14 15 14 13) #(0 0 0 0 0 0) 0 1))
(define hero-up-animation    (animated-sprite sprite-sheet #(24 25 26 27 26 25) #(0 0 0 0 0 0) 0 1))
(define hero-down-animation  (animated-sprite sprite-sheet #(36 37 38 39 38 37) #(0 0 0 0 0 0) 0 1))
; (define hero-debug-animation (animated-sprite sprite-sheet #( 4  4  4  4  4  4) #(0 0 0 0 0 0) 0 1)) 

(define   red-chased-animation (animated-sprite sprite-sheet #(49 50) #(4 4) 0 1))
(define green-chased-animation (animated-sprite sprite-sheet #(61 62) #(4 4) 0 1))
(define  blue-chased-animation (animated-sprite sprite-sheet #(73 74) #(4 4) 0 1))
(define brown-chased-animation (animated-sprite sprite-sheet #(85 86) #(4 4) 0 1))

(define red-actor-sprite   48) 
(define green-actor-sprite 60)
(define blue-actor-sprite  72)
(define brown-actor-sprite 84)


;;;
;;; DIRECTIONS
;;;

(define directions '(up down left right))

(define (neighbour id dir)
  (case dir
    [(#f)    id]
    [(up)    (up    id)]
    [(down)  (down  id)]
    [(left)  (left  id)]
    [(right) (right id)]
    [else    (raise-arguments-error 'neighbour
                                    "expected either #f or a direction 'up, 'down, 'left or 'right"
                                    "dir" dir)]))

(define (opposite-dir dir)
  (case dir
    [(#f)    #f]
    [(up)    'down]
    [(down)  'up]
    [(left)  'right]
    [(right) 'left]))


;;;
;;; Score
;;;

(define score 0)

(define (add-score! event)
  (case event
    [(dot)    (+= score 10)]
    [(pellet) (+= score 50)]))

;;;
;;; Actor Modes
;;;

; Modes:
;  chase       attempt to capture the hero
;  scatter     return to home corner
;  frightened  randomized

(define actor-mode 'scatter)

(define actor-mode-ticks (* 10 FRAME-RATE))  ;

(define (reset-actor-mode)
  (:= actor-mode-ticks (* 7 FRAME-RATE))
  (:= actor-mode 'chase))

(define (update-actor-mode-tick)
  (if (> actor-mode-ticks 0)
      (-- actor-mode-ticks)
      (case actor-mode
        [(chase)      (:= actor-mode 'scatter)
                      (:= actor-mode-ticks (* 20 FRAME-RATE))]
        [(scatter)    (:= actor-mode 'chase)
                      (:= actor-mode-ticks (* 7 FRAME-RATE))]
        [(frightened) (:= actor-mode 'scatter)
                      (:= actor-mode-ticks (* 10 FRAME-RATE))])))

(define (change-actor-mode-to-frightened)
  (:= actor-mode 'frightened)
  (:= actor-mode-ticks (* 10 FRAME-RATE))
  (for ([a the-actors])
    (:= a.dir (opposite-dir a.dir))))

;;;
;;; Level
;;;

(define level 1)

(define (reset-level!)
  (:= level 1))

(define (new-level!)
  (reset-bonus-items!)
  (+= level 1))


;;;
;;; Speed
;;;

; Each actor will get a slow-down count.
; Each frame it is decremented.
; When it hits zero, a frame is skipped.
; Say the slow-down count is set to 20, then 1:20 ~ 5% frames are skipped.

; The level and actor-mode can affect the 

(define (reset-actor-slow-count! a)
  (case level
    [(1) (case actor-mode
           [(frightened)    (:= a.slow-count 2)]    ; 50%
           [(chase scatter) (:= a.slow-count 5)])]  ; 25%
    [(2) (case actor-mode
           [(frightened)    (:= a.slow-count 3)]    ; 33%
           [(chase scatter) (:= a.slow-count 11)])] ; 10%
    [else (case actor-mode
           [(frightened)    (:= a.slow-count 3)]    ; 33%
           [(chase scatter) (:= a.slow-count 21)])] ;  5%
    ))

(define (reset-hero-slow-count! h)
  (case level
    [(1)  (:= h.slow-count 6)]       ; 80 %
    [(2)  (:= h.slow-count 11)]      ; 90 %
    [else (:= h.slow-count 10001)])) ; ~ 100 %

;;;
;;; Actors
;;;


(struct actor 
  (pos           ; a cell id
   offset        ; a vector of two integers
   dir           ; current direction: #f means no direction
   home-pos      ; the position to seek in scatter mode
   slow-count    ; skip one frame of movement, when zero
   sprite        ; currently displayed sprite or animated sprite
   normal-sprite ;
   chased-sprite ; used when the hero as eaten an energizer pellet
   next-dir      ; actor -> direction   called at turning points.       
   ))

(define (random-from-list xs)  
  (list-ref xs (inexact->exact (floor (random (length xs))))))

(define (compute-random-dir a)
  ; Pick a random turning direction.
  ; If none of the turning directions are available,
  ; go in the opposite direction.
  (define same       (list a.dir (opposite-dir a.dir)))
  (define turns      (remove* same '(left right up down)))
  (define candidates (filter (λ (dir) (accessible? (neighbour a.pos dir))) turns))
  (if (empty? candidates)
      (if (accessible? (neighbour a.pos a.dir))
          a.dir
          (opposite-dir a.dir))
      (random-from-list (if (empty? candidates) same turns))))


;; (define (put-turns-first dir dirs)
;;   (define (put-last dir dirs)
;;     (append (remove dir dirs) (list dir)))
;;   (define opp (opposite-dir dir))
;;   (put-last opp (put-last dir dirs)))

(define (compute-straight-line-dir a [target the-hero.pos])
  ; Move in the direction that makes the euclidean distance to the hero smallest
  (define candidates (filter (λ (dir) (accessible? (neighbour a.pos dir)))
                             '(left right down up)))
  (define dists      (for/list ([dir candidates])
                       (define-values (i j) (id->ij (neighbour a.pos dir)))
                       (define-values (r s) (id->ij target))
                       (+ (expt (- i r) 2) (expt (- j s) 2))))
  (define best       (apply min dists))
  (define k          (index-of dists best))
  (define dir        (list-ref candidates k))
  dir)

(define (compute-four-ahead-dir a)
  ; Move in the direction that makes the euclidean distance to the hero smallest
  ; to the target - which is the four tiles ahead of the hero.
  (define (neighbour* id dir) (or (and id (neighbour id dir)) id))
  (define target (for/fold ([pos the-hero.pos]) ([_ 4])
                   (neighbour* pos the-hero.dir)))
  (define candidates (filter (λ (dir) (accessible? (neighbour* a.pos dir)))
                             '(left right up down)))
  (define dists      (for/list ([dir candidates])
                       (define-values (i j) (id->ij (neighbour* a.pos dir)))
                       (define-values (r s) (id->ij target))
                       (+ (expt (- i r) 2) (expt (- j s) 2))))
  (define best       (apply min dists))
  (define k          (index-of dists best))
  (define dir        (list-ref candidates k))
  dir)

(define (compute-surround-dir a other-a)
  ; The direction of a depends on both the hero and the other actor.
  ; The idea is to approach the hero from the opposite direction
  ; that other-a is approaching the hero.

  ; We compute where the hero is two steps (if he continues in that direction).
  ; The we compute the tile on the other side of the hero (opposite to other-a).  
  (define (neighbour* id dir) (or (and id (neighbour id dir)) id))
  
  (define hero-target    (for/fold ([pos the-hero.pos]) ([_ 2])
                           (neighbour* pos the-hero.dir)))
  (define-values (Δi Δj) (id-delta other-a.pos hero-target))
  (define target         (id-add-ij other-a.pos (* 2 Δi) (* 2 Δj)))
  
  (define candidates (filter (λ (dir) (accessible? (neighbour* a.pos dir)))
                             '(left right up down)))
  (define dists      (for/list ([dir candidates])
                       (define-values (i j) (id->ij (neighbour* a.pos dir)))
                       (define-values (r s) (id->ij target))
                       (+ (expt (- i r) 2) (expt (- j s) 2))))
  (define best       (apply min dists))
  (define k          (index-of dists best))
  (define dir        (list-ref candidates k))
  dir)



         
(define (mr-random home-pos normal-sprite chased-sprite)
  (actor 348 
         (vector 0 0)
         'right
         home-pos
         20 ; slow count
         normal-sprite
         normal-sprite
         chased-sprite         
         (λ (a)
           (case actor-mode
             [(scatter)            (compute-straight-line-dir a a.home-pos)]
             [(frightened chase)   (compute-random-dir a)]
             [else                 (compute-straight-line-dir a a.home-pos)]))))

(define (mr-seek home-pos normal-sprite chased-sprite)
  (actor 349 
         (vector 0 0)
         'right
         home-pos
         20 ; slow count
         normal-sprite
         normal-sprite
         chased-sprite         
         (λ (a)
           (case actor-mode             
             [(scatter)    (compute-straight-line-dir a a.home-pos)]
             [(chase)      (compute-straight-line-dir a the-hero.pos)]
             [(frightened) (compute-random-dir a)]
             [else         (compute-straight-line-dir a a.home-pos)]))))

(define (mr-four-ahead home-pos normal-sprite chased-sprite)
  (actor 350 
         (vector 0 0)
         'right
         home-pos
         20 ; slow count
         normal-sprite
         normal-sprite
         chased-sprite         
         (λ (a)
           (case actor-mode             
             [(scatter)    (compute-straight-line-dir a a.home-pos)]
             [(chase)      (compute-four-ahead-dir a)]
             [(frightened) (compute-random-dir a)]
             [else         (compute-four-ahead-dir a)]))))

(define (mr-surround home-pos normal-sprite chased-sprite other-actor)
  (actor 351 
         (vector 0 0)
         'right
         home-pos
         20 ; slow count
         normal-sprite
         normal-sprite
         chased-sprite         
         (λ (a)
           (case actor-mode             
             [(scatter)    (compute-straight-line-dir a a.home-pos)]
             [(chase)      (compute-surround-dir a other-actor)]
             [(frightened) (compute-random-dir a)]
             [else         (compute-surround-dir a other-actor)]))))

                          
(define the-actors '())

(define (reset-actors!)
  (define seeker (mr-seek  614   red-actor-sprite   red-chased-animation))
  (:= the-actors
      (list seeker
            (mr-surround     29  blue-actor-sprite  blue-chased-animation seeker)
            (mr-four-ahead   54 green-actor-sprite green-chased-animation)
            (mr-random      589 brown-actor-sprite brown-chased-animation))))

(reset-actors!)
  


(define (draw-actor a)
  (define s a.sprite)
  (define-values (i j) (id->ij a.pos))
  (define x (+ (* 8 j) -4 a.offset.x))
  (define y (+ (* 8 i) -4 a.offset.y))
  (cond
    [(animated-sprite? s) (draw-animated-sprite s x y)]
    [(integer? s)         (draw-sprite s x y)]))

(define (update-actor a)
  (define skip-frame? #f)
  
  ;; the actor mode determines the sprites used
  (case actor-mode
    [(chase scatter) (:= a.sprite a.normal-sprite)]
    [(frightened)    (:= a.sprite a.chased-sprite)]
    [else            (:= a.sprite a.normal-sprite)])
    
  ;; Update sprite
  (when (animated-sprite? a.sprite)
    (update-animated-sprite a.sprite))

  ;; Are we skipping this frame?
  (-= a.slow-count 1)
  (when (= a.slow-count 0)
    (:= skip-frame? #t)
    (reset-actor-slow-count! a))
  
  ;; update position and offset  
  (when (and a.dir (not skip-frame?))
    (define dir         a.dir)
    (define opp-dir     (opposite-dir dir))
    (define next-dir    (let ([nd a.next-dir]) (nd a)))
    
    (define turning?    (and next-dir dir
                             (not (eq? next-dir dir))
                             (not (eq? next-dir opp-dir))))
    
    (define next        (and next-dir turning? (neighbour a.pos next-dir)))
    (define next-same   (and dir (neighbour a.pos dir)))
    (define next-opp    (and dir (neighbour a.pos opp-dir)))

    (define next-free?  (and next (accessible? next)))

    (define turning-point?
      (case a.dir
        [(left right) (= a.offset.x 0)]
        [(up   down)  (= a.offset.y 0)]))
    
    ; update the direction
    (cond
      [(and turning? turning-point? next-free?)
       ; we only turn if the offset is 0 (to stay in the middle)
       (:= a.dir next-dir)]
    
      [(and turning-point? (not (accessible? next-same)))
       (:= a.dir opp-dir)])

    ; update the offset
    (case a.dir
      [(left)  (when (or (> a.offset.x 0) (accessible? next-same))
                 (-= a.offset.x 1))]
      [(right) (when (or (< a.offset.x 0) (accessible? next-same))
                 (+= a.offset.x 1))]
      [(up)    (when (or (> a.offset.y 0) (accessible? next-same))
                 (-= a.offset.y 1))]
      [(down)  (when (or (< a.offset.y 0) (accessible? next-same))
                 (+= a.offset.y 1))])

    ; update the position
    (when (< a.offset.x -3)
      (cond
        [(accessible? (left a.pos)) (:= a.pos (left a.pos))
                                    (+= a.offset.x 8)]
        [else                       (:= a.offset.x -3)]))
    (when (> a.offset.x 4)
      (cond
        [(accessible? (right a.pos)) (:= a.pos (right a.pos))
                                     (-= a.offset.x 8)]
        [else                        (:= a.offset.x 4)]))
    (when (< a.offset.y -3)
      (cond
        [(accessible? (up a.pos))    (:= a.pos (up a.pos))
                                     (+= a.offset.y 8)]
        [else                        (:= a.offset.y -3)]))
    (when (> a.offset.y 4)
      (cond
        [(accessible? (down a.pos))  (:= a.pos (down a.pos))
                                     (-= a.offset.y 8)]
        [else                        (:= a.offset.y 4)])))
  )


;;;
;;; Hero
;;;

(struct hero 
  (pos        ; a cell id
   offset     ; a vector of two integers
   dir        ; current direction: #f means no direction
   slow-count ; decremented each frame, when zero a frame is skipped
   sprite     ; sprite or animated sprite   
   next-dir)) ; direction to move when possible

(define (reset-hero!)
  (:= the-hero
      (hero the-hero-start-pos    ; pos
            (vector 0 0)          ; offset    (mutable)  -4..3 x -4..3
            'right                ; direction
            6                     ; 80%
            hero-right-animation  ; sprite
            #f))                  ; next direction
  (reset-hero-slow-count! the-hero)) ; based on the level

(define the-hero-start-pos 686) ; the middle of last row
(define the-hero #f)
(reset-hero!)



(define (draw-hero h)
  (define as h.sprite)
  (define-values (i j) (id->ij h.pos))
  (define x (+ (* 8 j) -4 h.offset.x))
  (define y (+ (* 8 i) -4 h.offset.y))
  (draw-animated-sprite as x y))

       
(define (update-hero h)
  (define skip-frame? #f)

  (when (and (not h.dir) h.next-dir)
    (when (accessible? (neighbour h.pos h.next-dir))
      (:= h.dir h.next-dir)
      (:= h.next-dir #f)))
  
  ;; update the animation
  (update-animated-sprite h.sprite)

  ;; update the slow down count
  (-= h.slow-count 1)
  (when (= h.slow-count 0)
    (:= skip-frame? #t)
    (reset-hero-slow-count! h))

  ;; update position and offset
  (when (and (or h.dir h.next-dir)
             (not skip-frame?))
    (when (eq? h.next-dir h.dir) (:= h.next-dir #f))
    (define dir         h.dir)
    (define opp-dir     (opposite-dir h.dir))
    (define next-dir    h.next-dir)
    
    (define turning?    (and h.next-dir h.dir
                             (not (eq? h.next-dir h.dir))
                             (not (eq? h.next-dir opp-dir))))
    
    (define next        (and h.next-dir turning? (neighbour h.pos h.next-dir)))
    (define next-same   (and h.dir (neighbour h.pos h.dir)))
    (define next-opp    (and h.dir (neighbour h.pos opp-dir)))

    (define next-free?  (and next (accessible? next)))

    ; update the direction
    (cond
      [(eq? h.next-dir opp-dir)
       (:= h.dir opp-dir)
       (:= h.next-dir #f)]
      [turning?
       ; we only turn if the offset is 0 (to stay in the middle)
       (define turning-point?
         (case h.dir
           [(left right) (= h.offset.x 0)]
           [(up   down)  (= h.offset.y 0)]))
       (when (and turning? turning-point? next-free?)
         (:= h.dir h.next-dir)
         (:= h.next-dir #f))])

    ; update the offset
    (define (do-preturn)
      (case h.next-dir
        [(left)  (-= h.offset.x 1)]
        [(right) (+= h.offset.x 1)]
        [(up)    (-= h.offset.y 1)]
        [(down)  (+= h.offset.y 1)]))
    
    (case h.dir
      [(left)  (when (or (> h.offset.x 0) (accessible? next-same))
                 (-= h.offset.x 1))
               (when (and (> h.offset.x 0) next-free?)
                 (do-preturn))]
      [(right) (when (or (< h.offset.x 0) (accessible? next-same))
                 (+= h.offset.x 1))
               (when (and (< h.offset.x 0) next-free?)
                 (do-preturn))]
      [(up)    (when (or (> h.offset.y 0) (accessible? next-same))
                 (-= h.offset.y 1))
               (when (and (> h.offset.y 0) next-free?)
                 (do-preturn))]
      [(down)  (when (or (< h.offset.y 0) (accessible? next-same))
                 (+= h.offset.y 1))
               (when (and (< h.offset.y 0) next-free?)
                 (do-preturn))])

    ; update the position
    (when (< h.offset.x -3)
      (cond
        [(accessible? (left h.pos)) (:= h.pos (left h.pos))
                                    (+= h.offset.x 8)]
        [else                       (:= h.offset.x -3)]))
    (when (> h.offset.x 4)
      (cond
        [(accessible? (right h.pos)) (:= h.pos (right h.pos))
                                     (-= h.offset.x 8)]
        [else                        (:= h.offset.x 4)]))
    (when (< h.offset.y -3)
      (cond
        [(accessible? (up h.pos))    (:= h.pos (up h.pos))
                                     (+= h.offset.y 8)]
        [else                        (:= h.offset.y -3)]))
    (when (> h.offset.y 4)
      (cond
        [(accessible? (down h.pos))  (:= h.pos (down h.pos))
                                     (-= h.offset.y 8)]
        [else                        (:= h.offset.y 4)])))
  
  ; Now the position has been updated.

  ; detect dots and pellets
  (define eaten
    (cond [(set-member? (ann h.pos) 'dot)    'dot]
          [(set-member? (ann h.pos) 'pellet) 'pellet]
          [else         #f]))
  (when eaten
    (remove-ann h.pos eaten)
    (add-score! eaten)
    (define-values (i j) (id->ij h.pos))
    (define ts maze-sheet)
    (define black-tile-id 146) ; solid!
    (with-dc maze-dc
      (draw-tile ts black-tile-id (* j ts.width) (* i ts.height)))
    (play-sound 'dot-eaten))
  (when (eq? eaten 'pellet)
    (change-actor-mode-to-frightened))
  (when (eq? eaten 'dot)
    (-= dots-left-to-eat 1))

  ; have we collided with an actor?
  (case actor-mode
    [(frightened) ; the hero has eaten an energizer pellet recently
     (for ([a the-actors])
       (when (= a.pos h.pos)
         ; the hero has eaten an actor
         (:= a.pos 349)
         (:= a.dir 'right)))]
    [else
     ; the player died
     (for ([a the-actors])
       (when (= a.pos h.pos)
         ; the hero died!
         (cond
           [(> lives 0)
            (set-game-mode 'get-ready)
            (-= lives 1)]
           [(= lives 0)
            (set-game-mode 'game-over)])
         
         (:= h.pos      the-hero-start-pos)
         (:= h.offset.x 0)
         (:= h.offset.y 0)         
         (:= the-hero.dir      #f)
         (:= the-hero.next-dir #f)))])
  
  ;; pick the correct sprite animation
  ;; (note this needs to happen after the direction has been update)
  (case h.dir
    [(left)  (:= h.sprite hero-left-animation)]
    [(right) (:= h.sprite hero-right-animation)]
    [(down)  (:= h.sprite hero-down-animation)]
    [(up)    (:= h.sprite hero-up-animation)]))


;;;
;;; Dots, fruits other bonus items
;;;

(define dots-left-to-eat dots-in-level-count)

(define (reset-bonus-items!)
  (:= dots-left-to-eat dots-in-level-count))


;;;
;;; Modes
;;;

;   attract-screen
;   get-ready
;   playing
;   game-over

; Use  set-game-mode  to switch mode.

(define game-mode 'attract-screen)

(define (set-game-mode new-mode)
  (define new
    (case game-mode
      [(attract-screen) (case new-mode
                          [(get-ready) new-mode])]
      [(get-ready)      (case new-mode
                          [(playing)   new-mode])]
      [(playing)        (case new-mode
                          [(get-ready game-over) new-mode])]
      [(game-over)      (case new-mode
                          [(attract-screen) new-mode])]))
  (when (void? new)
    (raise-arguments-error 'set-game-mode "can't switch mode"
                           "old mode" game-mode
                           "new mode" new-mode))
  (:= game-mode new-mode))


;;;
;;; Lives
;;;

(define lives 3)

(define (reset-lives!)
  (:= lives 3))

;;;
;;; PALETTE
;;;

; The standard colors are:

(define red-color   (color 190  38  51))
(define blue-color  (color   0  87 132))
(define green-color (color  68 137  26))
(define brown-color (color 164 100  34))
(define white-color (color 255 255 255))
(define black-color (color   0   0   0))

;;;
;;; COLOR CYCLES
;;;

(struct color-cycle
  (colors  ; vector of colors
   times   ; how many frames to display each color is displayed
   current ; index into colors
   left))  ; how many frames left before current must be updated

(define (get-cycle-color cc)
  (vector-ref cc.colors cc.current))

(define (update-color-cycle cc)
  (define n (vector-length cc.colors))
  (case cc.left
    [(0)  (:= cc.current (modulo (+ cc.current 1) n))
          (:= cc.left (vector-ref cc.times cc.current))]
    [else (:= cc.left (- cc.left 1))]))


;;;
;;; Attract Screen
;;;

(define   red-attract (animated-sprite sprite-sheet #(49 50) #(4 4) 0 1))
(define green-attract (animated-sprite sprite-sheet #(61 62) #(4 4) 0 1))
(define  blue-attract (animated-sprite sprite-sheet #(73 74) #(4 4) 0 1))
(define brown-attract (animated-sprite sprite-sheet #(85 86) #(4 4) 0 1))

(define hero-attract  (animated-sprite sprite-sheet #( 0  1  2  3  2  1) #(0 0 0 0 0 0) 0 1))
(define hero-attract-x 0)

(define attract-sprites (list red-attract green-attract blue-attract brown-attract
                              hero-attract))

(define blinking     (color-cycle (vector white-color black-color)
                                  (vector 6           6)
                                  0 2))

(define (draw-attract-screen)
  (fill "red")
  (draw-text "    UNTITLED MAZE GAME"  0 (* 2 8)  white-color)

  ; (draw-text "    ENEMIES"             (* 5 8) (*  5 8) white-color)
  (draw-text "       SEEKER"           (* 5 8) (*  8 8) red-color)   
  (draw-text "       SURROUND"         (* 5 8) (* 11 8) blue-color)  
  (draw-text "       FOUR AHEAD"       (* 5 8) (* 14 8) green-color) 
  (draw-text "       RANDOM"           (* 5 8) (* 17 8) brown-color) 

  (draw-text "      SPACE TO START"          0 (* 24 8) (get-cycle-color blinking))

  (draw-animated-sprite red-attract   (+ (* 5 8) 30) (- (*  8 8) 4))
  (draw-animated-sprite blue-attract  (+ (* 5 8) 30) (- (* 11 8) 4))
  (draw-animated-sprite green-attract (+ (* 5 8) 30) (- (* 14 8) 4))
  (draw-animated-sprite brown-attract (+ (* 5 8) 30) (- (* 17 8) 4))

  (draw-animated-sprite hero-attract  (- hero-attract-x 32) (- (* 21 8) 4))
  (:= hero-attract-x (modulo (+ hero-attract-x 1) (+ SCREEN-WIDTH 64)))
  
  (for ([as attract-sprites]) (update-animated-sprite as))
  (update-color-cycle blinking))


;;;
;;; EVENT LOOP
;;;

(define (draw-maze)
  ; draw the maze on the bitmap `maze-bm`
  (with-dc maze-dc
    (background "black")
    (draw-tiles maze-sheet)))

(define (setup)
  (size ACTUAL-SCREEN-WIDTH ACTUAL-SCREEN-HEIGHT)
  ; (no-loop)
  (set-frame-rate! FRAME-RATE) ; default is 10
  (background "green")
  (image-mode 'corner)
  (draw-maze))

(define (on-key-pressed)
  (case game-mode
    [(attract-screen)
     (case key
       [(#\space)
        (set-game-mode 'get-ready)])]
    [(game-over)
     (case key
       [(#\space left right up down)
        (set-game-mode 'attract-screen)])]
    [(playing get-ready)
     (case key
       [(left right up down)
        (:= the-hero.next-dir key)]
       [(#\space)
        (:= the-hero.dir      #f)
        (:= the-hero.next-dir #f)]
       [(#\f)
        (:= actor-mode 'frightened)]
       [(#\c)
        (:= actor-mode 'chase)]
       [(#\s)
        (:= actor-mode 'scatter)]
       [(#\m)
        (:= mute-mode (not mute-mode))]
       [(#\d)
        (:= dots-left-to-eat 1)])
     (when (eq? game-mode 'get-ready)
       (set-game-mode 'playing))]))

(define (draw-fps)
  (define (format-score) (~a score #:min-width 6 #:pad-string " "))
  (fill "red")
  (draw-text (~a "LIVES "  lives
                 " LEVEL " level
                 " SCORE " (format-score)
                 ; " FPS "  (inexact->exact (floor frame-rate))
                 )
             8 8 "white"))

(define (update-playing)
  (update-actor-mode-tick)
  (update-hero the-hero)
  (for ([a the-actors])
    (update-actor a))

  ; ready for next level?
  (when (= dots-left-to-eat 0)
    (reset-level-data!)
    (reset-actors!)
    (reset-hero!)
    (draw-maze)
    (new-level!)
    (set-game-mode 'get-ready)))

(define (update-attract-screen)
  (reset-game-data!))  

(define (update-get-ready)
  (void))

(define (update-game-over)
  (void))

(define (reset-game-data!)
  (reset-lives!)
  (reset-actors!)
  (reset-hero!)
  (draw-maze)
  (reset-level-data!))


(define (draw)
  ;; Updates
  (case game-mode
    [(attract-screen) (update-attract-screen)]
    [(get-ready)      (update-get-ready)]
    [(playing)        (update-playing)]
    [(game-over)      (update-game-over)])
  
  ;; Reset screen-dc
  (send screen-dc set-transformation #(#(1. 0. 0. 1. 0. 0.) 0. 0. 1. 1. 0.))
  ;; Draw everything to the low resolution "screen".
  (with-dc screen-dc
    (background "black")
    (case game-mode

      [(attract-screen)
       (draw-attract-screen)]
      
      [(get-ready playing game-over)
       (draw-fps)
       ; Put (0,0) where the maze begins
       (translate 0 (* 8 TOP-ROWS))
       (image maze-bm 0 0)
       (image-mode 'corner)
       (draw-hero the-hero)
       (for ([a the-actors])
         (draw-actor a))
       (case game-mode
         [(get-ready) (draw-text "GET READY!" (* 10 8) (* 11 8) "white")]
         [(game-over) (draw-text "GAME OVER"  (* 10 8) (* 11 8) "white")])]))

  ;; Draw and scale the low resolution screen to the actual screen
  (scale SCALE)
  (image screen-bm 0 0))
