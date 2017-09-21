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
  (let ([redcircle (make-graphic 'redcircle (circle 3 "solid" "red") 4 7)]
        [bluerect (make-graphic 'bluerect (rectangle 5 25 "solid" "blue") 26 7)])
    (list (make-add redcircle)
          (make-add bluerect)
          (make-continued-action (make-collision-condition redcircle bluerect)
                                 (list (make-move redcircle (make-delta 7 2)))) 
          (make-delete bluerect) 
          (make-continued-action (make-edge-condition redcircle)
                                 (list (make-move redcircle (make-delta -7 -2))))
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
  (let ([orangecircle (make-graphic 'oranglecircle (circle 4 "solid" "orange") 8 7)]
    [greenrect (make-graphic 'greenrect (rectangle 35 5 "solid" "blue") 13 20)]
    [redrect (make-graphic 'redrect (rectangle 5 20 "solid" "red") 26 7)])
    (list (make-add orangecircle)
          (make-add greenrect)
          (make-continued-action (make-collision-condition orangecircle greenrect)
                                 (make-move orangecircle (make-delta 0 10))) 
          (make-add redrect)
          (make-continued-action (make-collision-condition orangecircle redrect)
                                 (make-move orangecircle (make-delta 10 0)))
          (make-jump orangecircle)
          )))

;; Example 4
(define EXAMPLE4
  (let ([redcircle (make-graphic 'redcircle (circle 6 "solid" "red") 0 20)]
        [bluecircle (make-graphic 'bluecircle (circle 6 "solid" "blue") 10 20)]
        [purplecircle (make-graphic 'purplecircle (circle 6 "solid" "purple") 20 20)])
    (list (make-add redcircle)
          (make-add bluecircle)
          (make-add purplecircle)
          (make-continued-action (make-collision-condition redcircle bluecircle)
                                 (make-move redcircle (make-delta 2 0)))
          (make-delete redcircle)
          (make-continued-action (make-collision-condition bluecircle purplecircle)
                                 (make-move bluecircle (make-delta 2 0)))
          (make-delete bluecircle)
          (make-continued-action (make-edge-condition purplecircle)
                                 (make-move purplecircle (make-delta 2 0)))
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
  (begin (update-frame (update-scene graphics))
         (sleep/yield 0.25)
         (cond [(move? a-cmd)
                (begin (do-move (move-shape a-cmd) (move-speed a-cmd)))]
               [(jump? a-cmd)
                (begin (do-jump (jump-shape a-cmd)))]
               [(delete? a-cmd)
                (begin (do-delete (delete-shape a-cmd)))]
               [(add? a-cmd)
                (begin (do-add (add-shape a-cmd)))]
               [(continued-action? a-cmd)
                (begin (do-continued-action a-cmd))])))

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
                                [else a-shape]))
                        graphics))))

;; do-move : graphic delta -> void
;; moves given graphic at given delta speed
(define (do-move a-shape a-delta)
  (begin
    (set! graphics (map (lambda (a-graphic)
                          (cond [(symbol=? (graphic-name a-graphic) (graphic-name a-shape))
                                 (make-graphic (graphic-name a-shape)
                                               (graphic-img a-shape)
                                               (+ (graphic-x a-shape) (delta-x a-delta))
                                               (+ (graphic-y a-shape) (delta-y a-delta)))]
                                [else a-shape]))
                        graphics))))

;; do-continued-action : continued-action -> void
;; does the list of actions until the given condition is true
(define (do-continued-action a-cmd)
  (begin
    (cond [(create-condition (continued-action-condit a-cmd))
           void]
          [else
           (begin (run-cmdlist (continued-action-cmds a-cmd))
                  (do-continued-action a-cmd))])))

;; create-condition : condition -> boolean
;; takes a condition and returns true if the collision occures
(define (create-condition a-condition)
  (cond [(collision-condition? a-condition)
         (create-collision-condition a-condition)]
        [(edge-condition? a-condition)
         (create-edge-condition a-condition)]))

;; create-edge-condition : condition -> boolean
;; consumes a condition and returns true if the passed shape collides with the edge of the scene
(define (create-edge-condition a-condition)
  (let ([w (image-width (graphic-img (lookup-graphic (edge-condition-shape a-condition))))]
        [h (image-height (graphic-img (lookup-graphic (edge-condition-shape a-condition))))]
        [x (graphic-x (lookup-graphic (edge-condition-shape a-condition)))]
        [y (graphic-y (lookup-graphic (edge-condition-shape a-condition)))])
    (or (< (- x (/ w 2)) 0)
        (> (+ x (/ w 2)) WIDTH)
        (< (- y (/ h 2)) 0)
        (> (+ y (/ h 2)) HEIGHT))))
#|
(check-expect (create-edge-condition (make-edge-condition g1)) true)
(check-expect (create-edge-condition (make-edge-condition g2)) true)
(check-expect (create-edge-condition (make-edge-condition g3)) true)
(check-expect (create-edge-condition (make-edge-condition g4)) true)

(define g1 (make-graphic 'g1 (circle 10 "solid" "blue") 250 0))
(define g2 (make-graphic 'g2 (circle 15 "solid" "red") 250 500))
(define g3 (make-graphic 'g3 (circle 30 "solid" "purple") 0 250))
(define g4 (make-graphic 'g4 (circle 30 "solid" "orange") 500 250))
|#

;; lookup-graphic : condition -> graphic
;; consumes a graphic and produces the graphic from the list "graphics" that corresponds
(define (lookup-graphic a-graphic)
  (first (filter (lambda (other-graphic)
                   (symbol=? (graphic-name a-graphic) (graphic-name other-graphic))) graphics)))

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