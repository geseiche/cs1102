;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hwk4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #t)))
;; Hwk 4 by Lucas Sacherer and Grace Seiche

;; an exam is a (make-exam list[cmd])
(define-struct exam (cmd-list))

;; a cmd is a
;; - (make-question string string symbol)
;; - (make-print-cmd string)
;; - (make-display-score string/symbol)
;; - (make-cond-skip number string/symbol list[cmd] list[cmd])
(define-struct question (que ans type))
(define-struct print-cmd (string))
(define-struct display-score (category)) ;; if category is 'all then find score of whole exam
(define-struct cond-skip (percent category lower higher))

;; an ans is a list of
;; - empty
;; - string
;; - number
;; - (make-ans-with-hint string string)
(define-struct ans-with-hint (answer hint))


(define math-exam
  (make-exam
   (list (make-question "What is 3*4+2?" (list 14) 'arithmetic)
         (make-question "What is 2+3*4?" (list 14) 'arithmetic)
         (make-question "What is 5+2*6?" (list 17) 'arithmetic)
         (make-cond-skip 50 'arithmetic
                         (list (make-print-cmd "You seem to be having trouble with these. Try again.")
                               (make-question "What is 3+5*2?" (list 13) 'arithmetic))
                         empty)
         (make-question "What is the reduced form of 12/18?: (1) 6/9 (2) 1/1.5 (3) 2/3"
                        (list 3) 'fractions)
         (make-cond-skip 60 'arithmetic
                         (list (make-question "What is 8+3*2?" (list 14) 'arithmetic))
                         (list (make-question "What is 1/4 + 1/2?: (1) 3/4 (2) 1/6 (3) 2/6"
                                              (list 1) 'fractions)))
         (make-display-score 'arithmetic)
         (make-display-score 'fractions)
         )))

(define wpi-history
  (make-exam
   (list (make-question "When was WPI founded?" "1865" 'history)
         (make-print-cmd "Let's see if you know your WPI personalitiies.")
         (make-question "What is Gompei?"
                        (list (make-ans-with-hint "goat" "Think bleating"))
                        'personalities)
         (make-question "Who was the first president of WPI? (1) Boynton (2) Washburn (3) Thompson"
                        (list 3) 'personalities)
         (make-display-score 'personalities)
         (make-question "Name one of the two towers behind a WPI education"
                        (list "theory" "practice")
                        'history)
         (make-display-score 'all)
         (make-print-cmd "There's more WPI history on the web. And life"))))
         