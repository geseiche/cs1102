;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
(define-struct graphic (name img x y))

(define-struct delta (x y))

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


(define (lookup-graphic a-graphic)
  (first (filter (lambda (other-graphic)
                   (symbol=? (graphic-name a-graphic) (graphic-name other-graphic)))
                 graphics)))

(define (move-shape a-shape a-delta log)
  (map (lambda (a-graphic)
                 (cond [(symbol=? (graphic-name a-graphic)
                                  (graphic-name (lookup-graphic a-shape)))
                        (make-graphic (graphic-name (lookup-graphic a-shape))
                                      (graphic-img (lookup-graphic a-shape))
                                      (+ (graphic-x (lookup-graphic a-shape))
                                         (delta-x a-delta))
                                      (+ (graphic-y (lookup-graphic a-shape))
                                         (delta-y a-delta)))]
                       [else a-graphic]))
               log))

(check-expect (move-shape g1 (make-delta -20 10) (list g1 g2 g3))
              (list (make-graphic 'g1 (circle 10 "solid" "blue") 230 10) g2 g3))
(check-expect (move-shape g7 (make-delta 10 -100) (list g3 g7 g12))
              (list g3 (make-graphic 'g7 (circle 20 "solid" "black") 30 150) g12))
(check-expect (move-shape i1 (make-delta 100 100) (list h1 h2 h3))
              (list (make-graphic 'h1 (circle 40 "solid" "red") 200 200) h2 h3))