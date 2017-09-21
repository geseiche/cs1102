;; Final Project: Phase 2
;; Grace Seiche

(require "world-cs1102.rkt")
(require test-engine/racket-tests)

;;-------------- LANGUAGE ------------------------

;; a delta is a (make-delta number number)
(define-struct delta (x y))

;; a graphic is a (make-graphic symbol image number number)
(define-struct graphic (name img x y))

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

;; Example 4a
(define EXAMPLE4a
  (let ([redcircle (make-graphic 'redcircle (circle 25 "solid" "red") 50 250)]
        [bluecircle (make-graphic 'bluecircle (circle 25 "solid" "blue") 200 250)]
        [purplecircle (make-graphic 'purplecircle (circle 25 "solid" "purple") 400 250)])
    (list (make-add redcircle)
          (make-add bluecircle)
          (make-add purplecircle)
          (make-continued-action (make-collision-condition redcircle bluecircle)
                                 (list (make-move redcircle (make-delta 10 0))
                                       (make-move bluecircle (make-delta 2 0))))
          (make-delete redcircle)
          (make-continued-action (make-collision-condition bluecircle purplecircle)
                                 (list (make-move bluecircle (make-delta 10 0))))
          (make-delete bluecircle)
          (make-continued-action (make-edge-condition purplecircle)
                                 (list (make-move purplecircle (make-delta 10 0))))
          )))

;;-------------- INTERPRETER ------------------------

;; graphics is a list[graphic]
(define graphics empty)

(define WIDTH 500)
(define HEIGHT 500)

;; run-animation: animation -> void
;; runs the passed animation and displays it for the user to see
(define (run-animation an-animation)
  (begin (create-canvas WIDTH HEIGHT)
         (set! graphics empty)
         (run-cmdlist an-animation)))

;; run-cmdlist: list[cmd] -> void
;; runs the list of commands and displays it for the user to see
(define (run-cmdlist an-animation)
  (for-each run-cmd an-animation))

;; run-cmd : cmd -> void
;; runs the command and produces a scene
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

;; do-add : graphic -> void
;; adds the graphic to the scene
(define (do-add a-shape)
  (begin
    (set! graphics (cons a-shape graphics))))

;; do-delete : graphic-> void
;; removes the graphic from the scene
(define (do-delete a-shape)
  (begin
    (set! graphics
          (filter (lambda (a-graphic)
                    (not (symbol=? (graphic-name a-shape) (graphic-name a-graphic))))
                    graphics))))

;; do-jump : graphic -> void
;; moves given graphic to a random location
(define (do-jump a-shape)
  (begin
    (set! graphics (map (lambda (a-graphic)
                          (cond [(symbol=? (graphic-name a-graphic) (graphic-name a-shape))
                                 (make-graphic (graphic-name a-shape)
                                               (graphic-img a-shape)
                                               (random WIDTH)
                                               (random HEIGHT))]
                                [else a-graphic]))
                        graphics))))

;; do-move : graphic delta -> void
;; moves given graphic at given delta speed
(define (do-move a-shape a-delta)
  (begin
    (set! graphics (map (lambda (a-graphic)
                          (cond [(symbol=? (graphic-name a-graphic) (graphic-name (lookup-graphic a-shape)))
                                 (make-graphic (graphic-name (lookup-graphic a-shape))
                                               (graphic-img (lookup-graphic a-shape))
                                               (+ (graphic-x (lookup-graphic a-shape)) (delta-x a-delta))
                                               (+ (graphic-y (lookup-graphic a-shape)) (delta-y a-delta)))]
                                [else a-graphic]))
                        graphics))))

;; do-continued-action : continued-action -> void
;; does the list of actions until the given condition is true
(define (do-continued-action a-cmd)
  (begin
    (run-cmdlist (continued-action-cmds a-cmd))
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

;;(set! graphics (list ))

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
                  (>= (+ x2 (/ w2 2)) (- x1 (/ w1 2))))
             (and (<= (- x2 (/ w2 2)) (+ x1 (/ w1 2)))
                  (>= (+ x2 (/ w2 2)) (+ x1 (/ w1 2)))))
         (or (and (<= (- y1 (/ h1 2)) (- y2 (/ h2 2)))
                  (>= (+ y1 (/ h1 2)) (- y2 (/ h2 2))))
             (and (<= (- y1 (/ h1 2)) (+ y2 (/ h2 2)))
                  (>= (+ y1 (/ h1 2)) (+ y2 (/ h2 2))))
             (and (<= (- y2 (/ h2 2)) (- y1 (/ h1 2)))
                  (>= (+ y2 (/ h2 2)) (- y1 (/ h1 2))))
             (and (<= (- y2 (/ h2 2)) (+ y1 (/ h1 2)))
                  (>= (+ y2 (/ h2 2)) (+ y1 (/ h1 2))))))))

(define h1 (make-graphic 'h1 (circle 40 "solid" "red") 100 100))
(define h2 (make-graphic 'h2 (circle 40 "solid" "red") 400 400))
(define h3 (make-graphic 'h3 (circle 40 "solid" "red") 140 140))
(define h4 (make-graphic 'h4 (circle 40 "solid" "red") 180 100))
(define h5 (make-graphic 'h5 (circle 10 "solid" "blue") 100 100))

(set! graphics (list g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 h1 h2 h3 h4 h5))

(check-expect (create-collision-condition (make-collision-condition h1 h2)) false)
(check-expect (create-collision-condition (make-collision-condition h1 h3)) true)
(check-expect (create-collision-condition (make-collision-condition h1 h4)) true)
(check-expect (create-collision-condition (make-collision-condition h1 h5)) true)

;; lookup-graphic : condition -> graphic
;; consumes a graphic and produces the graphic from the list "graphics" that corresponds
(define (lookup-graphic a-graphic)
  (first (filter (lambda (other-graphic)
                   (symbol=? (graphic-name a-graphic) (graphic-name other-graphic)))
                 graphics)))

;;-------------- INTERFACE HELPERS ------------------------

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