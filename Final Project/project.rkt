;; Final Project: Phase 4
;; Grace Seiche

#|

1. In order to run my program, you must type "(run-animation ex)" into the interactions
window where "ex" is
- EXAMPLE1
- EXAMPLE2
- EXAMPLE3
- EXAMPLE4
- EXAMPLE5
- EXAMPLE1m
- EXAMPLE2m
- EXAMPLE3m
- EXAMPLE4m
- EXAMPLE5m
Note: Animations EXAMPLE1 and EXAMPLE1m, EXAMPLE2 and EXAMPLE2m, etc. will display the
same animation. However, EXAMPLE1 is written in the original language and EXAMPLE1m is
written with macros.

2. I believe that my implementation is complete. Macros have been implemented. Block
commands work. Collisions function. All animations run as they are supposed to run. If
I were to change one thing, I would smooth the animations by decreasing the time between
scene updates and by decreasing the distance that the animations move between scenes.
This would make the animations less choppy. However, the animations are acceptable in
their current state.

3. I made many edits to my original language. First, I decided to remove separate
definitions for circles, ellipses, squares, and rectangles. When writing my interpreter,
it was very time consuming to determine whether the given graphic was a circle, ellipse,
square, or rectangle before being able to perform any functions. Therefore, I decided to
generalize circles, ellipses, squares, and rectangles to simply be graphics which have
a name, an image, an x-coordinate, and a y-coordinate. Originally, I had packaged the
x- and y-coordinates as posn structs, but it was too much of a hassle to go through yet
another layer to access what I needed. Also, I decided to classify continued-actions as
cmds rather than a lone struct because continued-action, like all commands, changes what
is produced in a scene. While this did not change the way any of the define-structs are
written, it influenced the way the functions in the interpreter were written because the
shape of the function follows the shape of the data.

4. Honestly, I think that I could change up my macros to be a little cleaner and a little
more user friendly. There are still many parentheses that could probably be removed somehow.
Also, within my interpreter, I believe it is possible to derive a simpler method to detect
graphic to graphic collisions. While it looks clean, there are many branches to the or and
and functions that could probably be reduced. However, I am not dissatified with the current
state of my implementation and I am very satisfied with the state of my design.

|#

(require "world-cs1102.rkt")
(require test-engine/racket-tests)

;;-------------- LANGUAGE ------------------------

;; a delta is a (make-delta number number)
(define-struct delta (x y))

;; a graphic is a (make-graphic symbol image number number)
(define-struct graphic (name img x y) (make-inspector))

;; a move is a (make-move graphic delta)
(define-struct move (shape speed))

;; a jump is a (make-jump graphic)
(define-struct jump (shape))

;; a delete is a (make-delete graphic)
(define-struct delete (shape))

;; an add is a (make-add graphic)
(define-struct add (shape))

;; a cmd is a
;;  - (make-move graphic delta)
;;  - (make-jump graphic)
;;  - (make-delete graphic)
;;  - (make-add graphic)
;;  - (make-continued-action condition list[cmd])

;; a continued-action is a (make-continued-action condition (list[cmd])
(define-struct continued-action (condit cmds))

;; a condition is a
;;  - collision-condition
;;  - edge-condition

;; a collision-condition (make-collision-condition graphic graphic)
(define-struct collision-condition (shape1 shape2))

;; an edge-condition (make-edge-condition graphic)
(define-struct edge-condition (shape))

;;-------------- EXAMPLES ------------------------

;; Example 1
(define EXAMPLE1
  (let ([redcircle (make-graphic 'redcircle (circle 25 "solid" "red") 20 27)]
        [bluerect (make-graphic 'bluerect (rectangle 50 400 "solid" "blue") 450 250)])
    (list (make-add redcircle)
          (make-add bluerect)
          (make-continued-action (make-collision-condition redcircle bluerect)
                                 (list (make-move redcircle (make-delta 7 2)))) 
          (make-delete bluerect) 
          (make-continued-action (make-edge-condition redcircle)
                                 (list (make-move redcircle (make-delta -7 2))))
          )))

;; Example 2
(define EXAMPLE2
  (let ([purplecircle (make-graphic 'purplecircle (circle 26 "solid" "purple") 54 74)])
    (list (make-add purplecircle)
          (make-continued-action (make-edge-condition purplecircle)
                                 (list (make-jump purplecircle)))
          )))

;; Example 3
(define EXAMPLE3
  (let ([orangecircle (make-graphic 'oranglecircle (circle 40 "solid" "orange") 50 50)]
    [greenrect (make-graphic 'greenrect (rectangle 400 50 "solid" "green") 250 450)]
    [redrect (make-graphic 'redrect (rectangle 50 350 "solid" "red") 450 225)])
    (list (make-add orangecircle)
          (make-add greenrect)
          (make-continued-action (make-collision-condition orangecircle greenrect)
                                 (list (make-move orangecircle (make-delta 0 10)))) 
          (make-add redrect)
          (make-continued-action (make-collision-condition orangecircle redrect)
                                 (list (make-move orangecircle (make-delta 10 0))))
          (make-jump orangecircle)
          )))

;; Example 4
(define EXAMPLE4
  (let ([redcircle (make-graphic 'redcircle (circle 35 "solid" "red") 50 250)]
        [bluecircle (make-graphic 'bluecircle (circle 35 "solid" "blue") 250 250)]
        [purplecircle (make-graphic 'purplecircle (circle 35 "solid" "purple") 400 250)])
    (list (make-add redcircle)
          (make-add bluecircle)
          (make-add purplecircle)
          (make-continued-action (make-collision-condition redcircle bluecircle)
                                 (list (make-move redcircle (make-delta 10 0))))
          (make-delete redcircle)
          (make-continued-action (make-collision-condition bluecircle purplecircle)
                                 (list (make-move bluecircle (make-delta 10 0))))
          (make-delete bluecircle)
          (make-continued-action (make-edge-condition purplecircle)
                                 (list (make-move purplecircle (make-delta 10 0))))
          )))

;; Example 5
(define EXAMPLE5
  (let ([redcircle (make-graphic 'redcircle (circle 25 "solid" "red") 50 250)]
        [redcircle2 (make-graphic 'redcircle2 (circle 25 "solid" "red") 450 250)]
        [redcircle3 (make-graphic 'redcircle3 (circle 25 "solid" "red") 250 50)]
        [redcircle4 (make-graphic 'redcircle4 (circle 25 "solid" "red") 250 450)]
        [purplecircle (make-graphic 'purplecircle (circle 25 "solid" "purple") 250 250)])
    (list (make-add redcircle)
          (make-add redcircle2)
          (make-add redcircle3)
          (make-add redcircle4)
          (make-add purplecircle)
          (make-continued-action (make-collision-condition redcircle purplecircle)
                                 (list (make-move redcircle (make-delta 10 0))
                                       (make-move redcircle2 (make-delta -10 0))
                                       (make-move redcircle3 (make-delta 0 10))
                                       (make-move redcircle4 (make-delta 0 -10))))
          (make-delete purplecircle)
          (make-continued-action (make-edge-condition redcircle)
                                 (list (make-move redcircle (make-delta -10 0))
                                       (make-move redcircle2 (make-delta 10 0))
                                       (make-move redcircle3 (make-delta 0 -10))
                                       (make-move redcircle4 (make-delta 0 10))))
          )))

;;-------------- MACROS ------------------------

;; macro to simplify definition of a circle image
(define-syntax a-circle
  (syntax-rules (radius)
    [(a-circle color radius num)
     (circle num "solid" (symbol->string 'color))]))

;; macro to simplify definition of a rectangle image
(define-syntax a-rectangle
  (syntax-rules (width height)
    [(a-rectangle color width num1 height num2)
     (rectangle num1 num2 "solid" (symbol->string 'color))]))

;; macro to replace make-delta with speed
(define-syntax speed
  (syntax-rules ()
    [(speed num1 num2)
     (make-delta num1 num2)]))

;; macro to replace make-add with create
(define-syntax create
  (syntax-rules ()
    [(create graphic)
     (make-add graphic)]))

;; macro to replace make-delete with remove
(define-syntax remove
  (syntax-rules ()
    [(remove graphic)
     (make-delete graphic)]))

;; macro to replace make-move with move
(define-syntax move
  (syntax-rules (at)
    [(move graphic at delta)
     (make-move graphic delta)]))

;; macro to replace make-jump with jump
(define-syntax jump
  (syntax-rules ()
      [(jump graphic)
       (make-jump graphic)]))

;; macro to replace make-continued-action with until
(define-syntax until
  (syntax-rules ()
      [(until condition actions)
       (make-continued-action condition actions)]))

;; macro to replace list with do
(define-syntax do
  (syntax-rules ()
    [(do cmd ...)
     (list cmd ...)]))

;; macro to replace make-collision-condition with collide
(define-syntax collide
  (syntax-rules ()
    [(collide graphic1 graphic2)
     (make-collision-condition graphic1 graphic2)]))

;; macro to replace make-edge-condition with hits-edge
(define-syntax hits-edge
  (syntax-rules ()
    [(hits-edge graphic)
     (make-edge-condition graphic)]))

;; macro to mask the let and simplify the graphic definition
(define-syntax shape
  (syntax-rules (is at)
    [(shape ([name is image at x y] ...) cmd)
     (let ([name (make-graphic 'name image x y)] ...) cmd)]))

;;-------------- EXAMPLES WITH MACROS ------------------------

;; Example 1 with macros
(define EXAMPLE1m
  (shape ([redcircle is (a-circle red radius 25) at 20 27]
        [bluerect is (a-rectangle blue width 50 height 400) at 450 250])
    (do (create redcircle)
      (create bluerect)
      (until (collide redcircle bluerect)
             (do (move redcircle at (speed 7 2)))) 
      (remove bluerect) 
      (until (hits-edge redcircle)
             (do (move redcircle at (speed -7 2))))
      )))

;; Example 2 with macros
(define EXAMPLE2m
  (shape ([purplecircle is (a-circle purple radius 26) at 54 74])
         (do (create purplecircle)
           (until (hits-edge purplecircle)
                  (do (jump purplecircle)))
           )))

;; Example 3 with macros
(define EXAMPLE3m
  (shape ([orangecircle is (a-circle orange radius 40) at 50 50]
          [greenrect is (a-rectangle green width 400 height 50) at 250 450]
          [redrect is (a-rectangle red width 50 height 350) at 450 225])
         (do (create orangecircle)
           (create greenrect)
           (until (collide orangecircle greenrect)
                  (do (move orangecircle at (speed 0 10)))) 
           (create redrect)
           (until (collide orangecircle redrect)
                  (do (move orangecircle at (speed 10 0))))
           (jump orangecircle)
           )))

;; Example 4 with macros
(define EXAMPLE4m
  (shape ([redcircle is (a-circle red radius 35) at 50 250]
          [bluecircle is (a-circle blue radius 35) at 250 250]
          [purplecircle is (a-circle purple radius 35) at 400 250])
         (do (create redcircle)
           (create bluecircle)
           (create purplecircle)
           (until (collide redcircle bluecircle)
                  (do (move redcircle at (speed 10 0))))
           (remove redcircle)
           (until (collide bluecircle purplecircle)
                  (do (move bluecircle at (speed 10 0))))
           (remove bluecircle)
           (until (hits-edge purplecircle)
                  (do (move purplecircle at (speed 10 0))))
           )))

;; Example 5 with macros
(define EXAMPLE5m
  (shape ([redcircle is (a-circle red radius 25) at 50 250]
        [redcircle2 is (a-circle red radius 25) at 450 250]
        [redcircle3 is (a-circle red radius 25) at 250 50]
        [redcircle4 is (a-circle red radius 25) at 250 450]
        [purplecircle is (a-circle purple radius 25) at 250 250])
    (do (create redcircle)
          (create redcircle2)
          (create redcircle3)
          (create redcircle4)
          (create purplecircle)
          (until (collide redcircle purplecircle)
                                 (do (move redcircle at (speed 10 0))
                                       (move redcircle2 at (speed -10 0))
                                       (move redcircle3 at (speed 0 10))
                                       (move redcircle4 at (speed 0 -10))))
          (remove purplecircle)
          (until (hits-edge redcircle)
                                 (do (move redcircle at (speed -10 0))
                                       (move redcircle2 at (speed 10 0))
                                       (move redcircle3 at (speed 0 -10))
                                       (move redcircle4 at (speed 0 10))))
          )))

;;-------------- INTERPRETER ------------------------

;; graphics is a list[graphic]
(define graphics empty)

(define WIDTH 500)
(define HEIGHT 500)

;; DEFINITIONS FOR CHECK-EXPECTS
(define g1 (make-graphic 'g1 (circle 10 "solid" "blue") 250 0))
(define g2 (make-graphic 'g2 (circle 15 "solid" "red") 250 500))
(define g3 (make-graphic 'g3 (circle 30 "solid" "purple") 0 250))
(define g4 (make-graphic 'g4 (circle 30 "solid" "orange") 500 250))
(define g5 (make-graphic 'g5 (circle 20 "solid" "black") 250 20))
(define g6 (make-graphic 'g6 (circle 20 "solid" "black") 250 480))
(define g7 (make-graphic 'g7 (circle 20 "solid" "black") 20 250))
(define g8 (make-graphic 'g8 (circle 20 "solid" "black") 480 250))
(define g9 (make-graphic 'g9 (circle 20 "solid" "black") 250 21))
(define g10 (make-graphic 'g10 (circle 20 "solid" "black") 250 479))
(define g11 (make-graphic 'g11 (circle 20 "solid" "black") 21 250))
(define g12 (make-graphic 'g12 (circle 20 "solid" "black") 479 250))
(define h1 (make-graphic 'h1 (circle 40 "solid" "red") 100 100))
(define h2 (make-graphic 'h2 (circle 40 "solid" "red") 400 400))
(define h3 (make-graphic 'h3 (circle 40 "solid" "red") 140 140))
(define h4 (make-graphic 'h4 (circle 40 "solid" "red") 180 100))
(define h5 (make-graphic 'h5 (circle 10 "solid" "blue") 100 100))
(define h6 (make-graphic 'h6 (circle 40 "solid" "red") 181 100))
(define i1 (make-graphic 'h1 (circle 10 "solid" "blue") 100 100))
(define i2 (make-graphic 'h2 (circle 40 "solid" "red") 400 400))
(set! graphics (list g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 h1 h2 h3 h4 h5 h6))

;; run-animation: animation -> void
;; runs the passed animation and displays it for the user to see
(define (run-animation an-animation)
  (begin (create-canvas WIDTH HEIGHT)
         (set! graphics empty)
         (run-cmdlist an-animation)))

;; run-cmdlist: list[cmd] -> void
;; runs the list of commands
(define (run-cmdlist an-animation)
  (for-each run-cmd an-animation))

;; run-cmd : cmd -> void
;; runs the command and displays it for the user to see
(define (run-cmd a-cmd)
  (begin (cond [(move? a-cmd)
                (begin (do-move (move-shape a-cmd) (move-speed a-cmd)))]
               [(jump? a-cmd)
                (begin (do-jump (jump-shape a-cmd)))]
               [(delete? a-cmd)
                (begin (do-delete (delete-shape a-cmd)))]
               [(add? a-cmd)
                (begin (do-add (add-shape a-cmd)))]
               [(continued-action? a-cmd)
                (begin (do-continued-action a-cmd))])
         (update-frame (update-scene graphics))
         (sleep/yield 0.25)))

;; run-cmdlist*: list[cmd] -> void
;; runs the list of commands and displays it for the user to see
(define (run-cmdlist* an-animation)
  (begin (for-each run-cmd* an-animation)
         (update-frame (update-scene graphics))
         (sleep/yield 0.25)))

;; run-cmd* : cmd -> void
;; runs the command and but does NOT show the user
(define (run-cmd* a-cmd)
  (begin (cond [(move? a-cmd)
                (begin (do-move (move-shape a-cmd) (move-speed a-cmd)))]
               [(jump? a-cmd)
                (begin (do-jump (jump-shape a-cmd)))]
               [(delete? a-cmd)
                (begin (do-delete (delete-shape a-cmd)))]
               [(add? a-cmd)
                (begin (do-add (add-shape a-cmd)))]
               [(continued-action? a-cmd)
                (begin (do-continued-action a-cmd))])
         ))

;; do-add : graphic -> void
;; adds the graphic to the scene
(define (do-add a-shape)
  (begin
    (set! graphics (cons a-shape graphics))))

;; do-delete : graphic-> void
;; removes the graphic from the scene
(define (do-delete a-shape)
  (begin
    (set! graphics (do-delete-helper a-shape graphics))))

;; do-delete-helper : graphic list[graphic] -> list[graphics]
;; takes a list[graphic] and a graphic and removes the graphic from the list
(define (do-delete-helper given-shape log)
  (filter (lambda (list-shape)
                    (not (symbol=? (graphic-name given-shape) (graphic-name list-shape))))
                    log))
(check-expect (do-delete-helper g2 (list g1 g2 g3)) (list g1 g3))
(check-expect (do-delete-helper i1 (list h1 h2 h3)) (list h2 h3))
(check-expect (do-delete-helper i2 (list g1 h2 h6)) (list g1 h6))

;; do-jump : graphic -> void
;; moves given graphic to a random location
(define (do-jump a-shape)
  (begin
    (set! graphics (jump-shape a-shape graphics))))

;; jump-shape : graphic list[graphic] -> list[graphic]
;; locates given graphic in the list of graphics and replaces it with the same graphic
;;  at a random position
(define (jump-shape given-shape log)
  (map (lambda (list-shape)
         (cond [(symbol=? (graphic-name list-shape) (graphic-name given-shape))
                (make-graphic (graphic-name given-shape)
                              (graphic-img given-shape)
                              (random WIDTH)
                              (random HEIGHT))]
               [else list-shape]))
       log))
;; no check-expects because produces a random position

;; do-move : graphic delta -> void
;; moves given graphic at given delta speed
(define (do-move a-shape a-delta)
  (begin
    (set! graphics (do-move-helper a-shape a-delta graphics)
          )))

;; do-move-helper : graphic delta list[graphic] -> list[graphic]
;; locates given graphic in the list of graphics and replaces it with the same graphic
;;  moved by the given delta
(define (do-move-helper given-shape a-delta log)
  (map (lambda (list-shape)
                 (cond [(symbol=? (graphic-name list-shape)
                                  (graphic-name (lookup-graphic given-shape)))
                        (make-graphic (graphic-name (lookup-graphic given-shape))
                                      (graphic-img (lookup-graphic given-shape))
                                      (+ (graphic-x (lookup-graphic given-shape))
                                         (delta-x a-delta))
                                      (+ (graphic-y (lookup-graphic given-shape))
                                         (delta-y a-delta)))]
                       [else list-shape]))
               log))
(check-expect (do-move-helper g1 (make-delta -20 10) (list g1))
              (list (make-graphic 'g1 (circle 10 "solid" "blue") 230 10)))
(check-expect (do-move-helper g7 (make-delta 10 -100) (list g3 g7 g12))
              (list g3 (make-graphic 'g7 (circle 20 "solid" "black") 30 150) g12))
(check-expect (do-move-helper i1 (make-delta 100 100) (list h1 h2 h3))
              (list (make-graphic 'h1 (circle 40 "solid" "red") 200 200) h2 h3))

;; do-continued-action : continued-action -> void
;; does the list of actions until the given condition is true
(define (do-continued-action a-cmd)
  (begin
    (run-cmdlist* (continued-action-cmds a-cmd))
    (cond [(create-condition (continued-action-condit a-cmd))
           void]
          [else
           (begin (do-continued-action a-cmd))])))

;; create-condition : condition -> boolean
;; takes a condition and returns true if the collision occures
(define (create-condition a-condition)
  (cond [(collision-condition? a-condition)
         (create-collision-condition a-condition)]
        [(edge-condition? a-condition)
         (create-edge-condition a-condition)]))
(check-expect (create-condition (make-edge-condition g8)) true)
(check-expect (create-condition (make-edge-condition g9)) false)
(check-expect (create-condition (make-collision-condition h1 h2)) false)
(check-expect (create-condition (make-collision-condition h1 h3)) true)

;; create-edge-condition : condition -> boolean
;; consumes a condition and returns true if the passed shape collides with the edge
;;   of the scene
(define (create-edge-condition a-condition)
  (let ([w (image-width (graphic-img (lookup-graphic (edge-condition-shape a-condition))))]
        [h (image-height (graphic-img (lookup-graphic (edge-condition-shape a-condition))))]
        [x (graphic-x (lookup-graphic (edge-condition-shape a-condition)))]
        [y (graphic-y (lookup-graphic (edge-condition-shape a-condition)))])
    (or (<= (- x (/ w 2)) 0)
        (>= (+ x (/ w 2)) WIDTH)
        (<= (- y (/ h 2)) 0)
        (>= (+ y (/ h 2)) HEIGHT))))
(check-expect (create-edge-condition (make-edge-condition g1)) true)
(check-expect (create-edge-condition (make-edge-condition g2)) true)
(check-expect (create-edge-condition (make-edge-condition g3)) true)
(check-expect (create-edge-condition (make-edge-condition g4)) true)
(check-expect (create-edge-condition (make-edge-condition g5)) true)
(check-expect (create-edge-condition (make-edge-condition g6)) true)
(check-expect (create-edge-condition (make-edge-condition g7)) true)
(check-expect (create-edge-condition (make-edge-condition g8)) true)
(check-expect (create-edge-condition (make-edge-condition g9)) false)
(check-expect (create-edge-condition (make-edge-condition g10)) false)
(check-expect (create-edge-condition (make-edge-condition g11)) false)
(check-expect (create-edge-condition (make-edge-condition g12)) false)

;; create-collision-condition : condition -> boolean
;; takes a condition and returns a boolean if the two shapes inside the condition have
;;   collided
(define (create-collision-condition a-condition)
  (let ([w1 (image-width (graphic-img (lookup-graphic
                                       (collision-condition-shape1 a-condition))))]
        [h1 (image-height (graphic-img (lookup-graphic
                                        (collision-condition-shape1 a-condition))))]
        [x1 (graphic-x (lookup-graphic (collision-condition-shape1 a-condition)))]
        [y1 (graphic-y (lookup-graphic (collision-condition-shape1 a-condition)))]
        [w2 (image-width (graphic-img (lookup-graphic
                                       (collision-condition-shape2 a-condition))))]
        [h2 (image-height (graphic-img (lookup-graphic
                                        (collision-condition-shape2 a-condition))))]
        [x2 (graphic-x (lookup-graphic (collision-condition-shape2 a-condition)))]
        [y2 (graphic-y (lookup-graphic (collision-condition-shape2 a-condition)))])
    (and (or (and (<= (- x1 (/ w1 2)) (- x2 (/ w2 2)))
                  (>= (+ x1 (/ w1 2)) (- x2 (/ w2 2))))
             (and (<= (- x1 (/ w1 2)) (+ x2 (/ w2 2)))
                  (>= (+ x1 (/ w1 2)) (+ x2 (/ w2 2))))
             (and (<= (- x2 (/ w2 2)) (- x1 (/ w1 2)))
                  (>= (+ x2 (/ w2 2)) (- x1 (/ w1 2)))))
         (or (and (<= (- y1 (/ h1 2)) (- y2 (/ h2 2)))
                  (>= (+ y1 (/ h1 2)) (- y2 (/ h2 2))))
             (and (<= (- y1 (/ h1 2)) (+ y2 (/ h2 2)))
                  (>= (+ y1 (/ h1 2)) (+ y2 (/ h2 2))))
             (and (<= (- y2 (/ h2 2)) (- y1 (/ h1 2)))
                  (>= (+ y2 (/ h2 2)) (- y1 (/ h1 2))))))))
(check-expect (create-collision-condition (make-collision-condition h1 h2)) false)
(check-expect (create-collision-condition (make-collision-condition h1 h3)) true)
(check-expect (create-collision-condition (make-collision-condition h1 h4)) true)
(check-expect (create-collision-condition (make-collision-condition h1 h5)) true)
(check-expect (create-collision-condition (make-collision-condition h1 h6)) false)
(check-expect (create-collision-condition (make-collision-condition h5 h1)) true)

;; lookup-graphic : graphic -> graphic
;; consumes a graphic and produces the graphic from the list "graphics" that corresponds
(define (lookup-graphic given-shape)
  (first (filter (lambda (list-graphic)
                   (symbol=? (graphic-name given-shape) (graphic-name list-graphic)))
                 graphics)))
(check-expect (lookup-graphic h3) h3)
(check-expect (lookup-graphic g6) g6)
(check-expect (lookup-graphic i2) h2)
(check-expect (lookup-graphic i1) h1)

;; update-scene : list[graphic] -> scene
;; places the graphics on a canvas
(define (update-scene shapes)
  (cond [(empty? shapes)
         (empty-scene WIDTH HEIGHT)]
        [(cons? shapes)
         (place-image (graphic-img (first shapes))
                      (graphic-x (first shapes))
                      (graphic-y (first shapes))
                      (update-scene (rest shapes)))]))

(test)