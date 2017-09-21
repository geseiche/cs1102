;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hwk1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Homework 1 for Lucas Sacherer and Grace Seiche

;; Question 1

;; an insert is (make-insert string)
(define-struct insert (str))
;; Examples of inserts
(define INSERT1 (make-insert " computer"))
(define INSERT2 (make-insert "this is a string"))
#|
    insert-fun : insert . . . -> . . . 
    (define (insert-fun an-insert)
      . . . (insert-str an-insert) . . .
     )
|#

;; a delete is (make-insert number)
(define-struct delete (num))
;; Examples of deletes
(define DELETE1 (make-delete 6))
(define DELETE2 (make-delete 7))
#|
     delete-fun : delete . . . -> . . . 
     (define (delete-fun a-delete)
       . . . (delete-num a-delete) . . . 
      )
|#

;; an operation is
;;  -an insert, or
;;  -a delete
#|
    operation-fun : operation . . . -> . . .
    (define (opertion-fun an-operation)
        (cond [(insert? an-operation)
                . . . (insert-str an-insert) . . .]
              [(delete? an-operation)
                . . . (delete-num a-delete) . . . ]
     ))
|#

;; a patch is (make-patch number operation)
(define-struct patch (pos op))
;; Examples of patches
(define PATCH1 (make-patch 9 INSERT1))
(define PATCH2 (make-patch 9 DELETE2))
(define PATCH3 (make-patch 3 INSERT1))
(define PATCH4 (make-patch 10 INSERT1))
(define PATCH5 (make-patch 2 DELETE1))
(define PATCH6 (make-patch 8 DELETE2))
#|
    patch-fun : patch . . . -> . . . 
    (define (patch-fun a-patch)
         . . . (patch-pos a-patch) . . .
         . . . (patch-op a-patch) . . . 
     )
|#


;; Question 2

;; apply-op : operation string number -> string
;; consumes an operation to perform, a document to be operated on,
;;   and a location to perform the operation and produces a document
;;   with the operation performed at the given location
(define (apply-op an-operation doc pos)
        (cond [(insert? an-operation)
               (string-append (substring doc 0 pos) (insert-str an-operation)
                              (substring doc pos (string-length doc)))]
              [(delete? an-operation)
               (string-append (substring doc 0 pos)
                              (substring doc (+ pos (delete-num an-operation))
                                         (string-length doc)))]
     ))
;; Test Cases
(check-expect (apply-op INSERT1 "This is a string" 9) "This is a computer string")
(check-expect (apply-op DELETE1 "This is a string" 10) "This is a ")


;; Question 3

;; apply-patch : patch string -> string
;; consumes a patch and a document and produces a document with the patch applied
(define (apply-patch a-patch doc)
         (apply-op (patch-op a-patch) doc (patch-pos a-patch))
  )
;; Test Cases
(check-expect (apply-patch PATCH1 "This is a string") "This is a computer string")
(check-expect (apply-patch PATCH2 "This is a string") "This is a")


;; Question 4

;; delete-overlap? : number delete number delete -> boolean
;; consumes a 2 sets of positions and deletes and determines whether the deletes will overlap
(define (delete-overlap? pos1 del1 pos2 del2)
  (cond
    [(= pos1 pos2) true]
    [(< pos1 pos2) (> (+ pos1 (delete-num del1)) pos2)]
    [(> pos1 pos2) (> (+ pos2 (delete-num del2)) pos1)]
    ))
(check-expect (delete-overlap? 0 DELETE1 9 DELETE2) false)
(check-expect (delete-overlap? 9 DELETE1 0 DELETE2) false)
(check-expect (delete-overlap? 0 DELETE1 2 DELETE2) true)
(check-expect (delete-overlap? 2 DELETE1 0 DELETE2) true)
(check-expect (delete-overlap? 2 DELETE1 2 DELETE2) true)

;; delete-insert-overlap? : number delete  number -> boolean
;; consumes a position of a delete, a delete, and a position of the insert and determines
;;   whether the insert will be placed in the section that is being deleted
(define (delete-insert-overlap? delpos del1 inspos)
  (cond
    [(= delpos inspos) false]
    [(< delpos inspos) (< inspos (+ delpos (delete-num del1)))]
    [(> delpos inspos) false]
    ))
(check-expect (delete-insert-overlap? 0 DELETE1 9) false)
(check-expect (delete-insert-overlap? 0 DELETE1 5) true)
(check-expect (delete-insert-overlap? 9 DELETE1 0) false)
(check-expect (delete-insert-overlap? 9 DELETE1 9) false)

;; overlap? : patch patch -> boolean
;; consumes two patches and returns if they overlap
(define (overlap? patch1 patch2)
  (cond
    [(and (= (patch-pos patch1) (patch-pos patch2))
          (and (insert? (patch-op patch1)) (insert? (patch-op patch2)))) true]
    [(and (delete? (patch-op patch1)) (delete? (patch-op patch2)))
     (delete-overlap? (patch-pos patch1) (patch-op patch1) (patch-pos patch2) (patch-op patch2))]
    [(and (insert? (patch-op patch1)) (insert? (patch-op patch2)))
     false]
    [(and (insert? (patch-op patch1)) (delete? (patch-op patch2)))
     (delete-insert-overlap? (patch-pos patch2) (patch-op patch2) (patch-pos patch1))]
    [(and (delete? (patch-op patch1)) (insert? (patch-op patch2)))
     (delete-insert-overlap? (patch-pos patch1) (patch-op patch1) (patch-pos patch2))]))
(check-expect (overlap? PATCH1 PATCH2) false)
(check-expect (overlap? PATCH1 PATCH1) true)
(check-expect (overlap? PATCH2 PATCH5) false)
(check-expect (overlap? PATCH2 PATCH6) true)
(check-expect (overlap? PATCH3 PATCH1) false)
(check-expect (overlap? PATCH3 PATCH5) true)
(check-expect (overlap? PATCH3 PATCH6) false)
(check-expect (overlap? PATCH5 PATCH3) true)
(check-expect (overlap? PATCH6 PATCH3) false)


;; Question 5

;; merge : string patch patch -> string or boolean
;; apply both patches
(define (merge doc patch1 patch2)
  (cond
    [(overlap? patch1 patch2) false]
    [(and (= (patch-pos patch1) (patch-pos patch2)) (delete? (patch-op patch1)))
     (apply-patch patch2 (apply-patch patch1 doc))]
    [(and (= (patch-pos patch1) (patch-pos patch2)) (delete? (patch-op patch2)))
     (apply-patch patch1 (apply-patch patch2 doc))]
    [(< (patch-pos patch1) (patch-pos patch2)) (apply-patch patch1 (apply-patch patch2 doc))]
    [(< (patch-pos patch2) (patch-pos patch1)) (apply-patch patch2 (apply-patch patch1 doc))]
    ))
(check-expect (merge "this is a string" PATCH2 PATCH6) false)
(check-expect (merge "this is a string" PATCH1 PATCH2) "this is a computer")
(check-expect (merge "this is a string" PATCH2 PATCH1) "this is a computer")
(check-expect (merge "this is a string" PATCH5 PATCH1) "tha computer string")
(check-expect (merge "this is a string" PATCH1 PATCH5) "tha computer string")


;; Question 6
#|
In the event of an overlap, returning false clearly tells the user that the
merge failed, instead of just returning a string that is the same as the input
string and may or may not have had the patches applied (and just coindcidentally
returned a string equivalent to the input string). Returning false is more clear
the merge did not occur.
|#


;; Question 7
(define ORIG "Hamlet: Do you see yonder cloud that's almost in shape of a camel?
Polonius: By the mass, and 'tis like a camel, indeed.
[...]
Hamlet: Or like a whale?
Polonius: Very like a whale.")
(define ALT "Hamlet: Do you see the cloud over there that's almost the shape of a camel?
Polonius: By golly, it is like a camel, indeed.
[...]
Hamlet: Or like a whale?
Polonius: It's totally like a whale.")
(define PA (make-patch 19 (make-delete 6)))
(define PB (make-patch 19 (make-insert "the")))
(define PC (make-patch 31 (make-insert " over there")))
(define PD (make-patch 46 (make-delete 2)))
(define PE (make-patch 46 (make-insert "the")))
(define PF (make-patch 80 (make-delete 8)))
(define PG (make-patch 80 (make-insert "golly")))
(define PH (make-patch 90 (make-delete 8)))
(define PI (make-patch 90 (make-insert "it is")))
(define PJ (make-patch 162 (make-delete 4)))
(define PK (make-patch 162 (make-insert "It's totally")))
;; modernize : string -> string
;; consumes a string and produces the string with the patches PA through PK applied
(define (modernize string)
  (merge (apply-patch PC (merge (merge (merge (merge string PJ PK) PH PI) PF PG) PD PE)) PA PB))
(check-expect (modernize ORIG) ALT)


#|
Question 8
(/ (- (* 9 3) (double 'a)) 2)
      ^^^^^^^
(/ (- 27 (double 'a)) 2)
          ^^^^^^^^^
(/ (- 27 (* 'a 2)) 2)
          ^^^^^^
*: expects a number as 1st argument, given 'a


Question 9
(or (< 5 2) (and (= 15 (- 18 3)) (> 8 4)))
                        ^^^^^^
(or (< 5 2) (and (= 15 15) (> 8 4)))
                  ^^^^^^^
(or (< 5 2) (and #true (> 8 4)))
                        ^^^^^
(or (< 5 2) (and #true #true))
    ^^^^^^^
(or #false (and #true #true))
            ^^^^^^^^^^^^^^^
(or #false #true)
^^^^^^^^^^^^^^^^^
#true


Question 10
(and (+ 5 -1) false)
      ^^^^^^
(and 4 false)
^^^^^^^^^^^^^
and: question result is not true or false: 4


Question 11
(apply-patch 'remove "this is a test string")
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
(apply-op (patch-op 'remove) "this is a test string" (patch-pos 'remove))
           ^^^^^^^^^^^^^^^^
patch-op: expects a patch, given 'remove


Question 12
This occurs when either the question or the answer is missing from a conditional
statement. (i.e. there is only one arguement between a set of square brackets in
the condition)
Example:
(cond [true])


Question 13
This error occurs when you try to use the variable x without defining its value
beforehand.
Example:
(+ x 2)


Question 14
This occurs when there is no function listed after the open parentesis and Racket
instead encounters a number.
Example:
(2)
|#