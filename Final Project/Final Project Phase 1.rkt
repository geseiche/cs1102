;; Final Project: Phase 1
;; Grace Seiche

(require "world-cs1102.rkt")

;;-------------- LANGUAGE ------------------------

;; a delta is a (make-delta number number)
(define-struct delta (x y))

;; a circ is an (make-circ number string pos)
(define-struct circ (radius color loc))

;; an elli is an (make-elli number number string pos)
(define-struct elli (length width color loc))

;; a squa is a (make-squa number string pos)
(define-struct squa (length color loc))

;; a rect is a (make-rect number number string pos)
(define-struct rect (length width color loc))

;; a graphic is a
;;  - circ
;;  - elli
;;  - squa
;;  - rect

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
(define-struct continued-action (func cmds))

;; a condition is a
;;  - collision-condition
;;  - edge-condition

;; a collision-condition (make-collision-condition graphic graphic)
(define-struct collision-condition (shape1 shape2))

;; an edge-condition (make-edge-condiiton graphic number number)
(define-struct edge-condition (shape length width))

;;-------------- EXAMPLES ------------------------

;; Example 1
(define EXAMPLE1
  (let ([redcircle (make-circ 3 "red" (make-posn 4 7))]
        [bluerect (make-rect 5 25 "blue" (make-posn 26 7))])
    (list (make-add redcircle)
          (make-add bluerect)
          (make-continued-action (make-collision-condition redcircle bluerect)
                                 (list (make-move redcircle (make-delta 7 2)))) 
          (make-delete bluerect) 
          (make-continued-action (make-edge-condition redcircle 300 100)
                                 (list (make-move redcircle (make-delta -7 -2))))
          )))

;; Example 2
(define EXAMPLE2
  (let ([purplecircle (make-circ 6 "purple" (make-posn 24 14))])
    (list (make-add purplecircle)
          (make-continued-action (make-edge-condition purplecircle 50 50)
                                 (list (make-jump purplecircle)))
          )))

;; Example 3
(define EXAMPLE3
  (let ([orangecircle (make-circ 4 "orange" (make-posn 8 7))]
    [greenrect (make-rect 35 5 "blue" (make-posn 13 20))]
    [redrect (make-rect 5 20 "red" (make-posn 26 7))])
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
  (let ([redcircle (make-circ 6 "red" (make-posn 0 20))]
        [bluecircle (make-circ 6 "blue" (make-posn 10 20))]
        [purplecircle (make-circ 6 "purple" (make-posn 20 20))])
    (list (make-add redcircle)
          (make-add bluecircle)
          (make-add purplecircle)
          (make-continued-action (make-collision-condition redcircle bluecircle)
                                 (make-move redcircle (make-delta 2 0)))
          (make-delete redcircle)
          (make-continued-action (make-collision-condition bluecircle purplecircle)
                                 (make-move bluecircle (make-delta 2 0)))
          (make-delete bluecircle)
          (make-continued-action (make-edge-condition purplecircle 30 40)
                                 (make-move purplecircle (make-delta 2 0)))
          )))

;;-------------- INTERPRETER ------------------------

;; graphics is a list[graphic]
(define graphics empty)

(define WIDTH 50)
(define HEIGHT 50)

;; run-animation: animation -> void
;; runs the passed animation and displays it for the user to see
(define (run-animation an-animation)
  (run-cmdlist an-animation))

;; run-cmdlist: list[cmd] -> void
;; runs the list of commands and displays it for the user to see
(define (run-cmdlist an-animation)
  (for-each run-cmd an-animation))

;; run-cmd : cmd -> void
;; runs the command and produces a scene
(define (run-cmd a-cmd)
  (cond [(move? a-cmd)
         (begin (do-move (cmd-shape a-cmd) (cmd-speed a-cmd)))]
        [(jump? a-cmd)
         (begin (do-jump (cmd-shape a-cmd)))]
        [(delete? a-cmd)
         (begin (do-delete (cmd-shape a-cmd)))]
        [(add? a-cmd)
         (begin (do-add (cmd-shape a-cmd)))]
        [(continued-action? a-cmd)
         (begin (do-continued-action a-cmd))]))

;; do-add : graphic -> void
;; adds the graphic to the scene
(define (do-add a-shape)
  (begin
    (set! graphics (cons graphics a-shape))))

;; do-delete : graphic-> void
;; removes the graphic from the scene
(define (do-delete a-shape)
  (begin
    (set! graphics (filter (not (equals? a-shape)) graphics))))

;; do-jump : graphic -> void
;; moves given graphic to a random location
(define (do-jump a-shape)
  (begin
    (set! graphics (map (cond [(equals? a-shape)
                               (jump-graphic a-shape)]
                              [else a-shape])
                        graphics))))

;; do-move : graphic delta -> void
;; moves given graphic at given delta speed
(define (do-move a-shape a-delta)
  (begin
    (set! graphics (map (cond [(equals? a-shape)
                               (move-graphic a-shape a-delta)]
                              [else a-shape])
                        graphics))))

;; jump-graphic : graphic -> graphic
(define (jump-graphic a-shape)
  (cond [(circ? a-shape)
         (make-circ (circ-radius a-shape)
                    (circ-color a-shape)
                    (make-posn (random WIDTH) (random HEIGHT)))]
        [(elli? a-shape)
         (make-elli (elli-length a-shape)
                    (elli-width a-shape)
                    (elli-color a-shape)
                    (make-posn (random WIDTH) (random HEIGHT)))]
        [(squa? a-shape)
         (make-squa (squa-length a-shape)
                    (squa-color a-shape)
                    (

;;-------------- INTERFACE HELPERS ------------------------