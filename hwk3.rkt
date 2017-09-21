;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hwk3) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; hwk3 by Lucas Sacherer and Grace Seiche

;; a file is a (make-file symbol number value)
(define-struct file (name size content))

(define FILE1 (make-file 'FILE1 0 empty))

;; a list[file] is either:
;; - empty, or
;; - (cons file list[file])
#|
;; lof-fun : list[file] . . . -> . . .
(define (lof-fun a-lof)
   (cond [(empty? a-lof) . . .]
         [(cons? a-lof) . . . (first a-lof) . . .
                        . . . (lof-fun (rest a-lof)) . . .]
    ))
|#

;; a dir is a (make-dir symbol list[dir] list[file])
(define-struct dir (name dirs files))

(define DIR1 (make-dir 'DIR1 empty empty))
(define DIR2 (make-dir 'DIR2 empty (list FILE1)))
#|
;; dir-fun : dir . . . -> . . .
(define (dir-fun a-dir)
   . . . (dir-name a-dir) . . .
   . . . (lod-fun (dir-dirs a-dir)) . . .
   . . . (lof-fun (dir-files a-dir)) . . .)
|#

;; a list[dir] is either:
;; - empty, or
;; - (cons dir list[dir])
#|
;; lod-fun : list[dir] . . . -> . . .
(define (lod-fun a-lod)
   (cond [(empty? a-lod) . . . ]
         [(cons? a-lod) . . . (dir-fun (first a-lod)) . . .
                        . . . (lod-fun (rest a-lod)) . . .]
    ))
|#

;; Question 1
(define FILE2 (make-file 'FILE2 20 "Hello World"))
(define FILE3 (make-file 'FILE3 3 empty))
(define FILE4 (make-file 'FILE4 15 empty))
(define FILE5 (make-file 'FILE5 6 empty))
(define FILE6 (make-file 'FILE6 10 "Hello World"))

(define D0 (make-dir 'D0 empty empty))
(define D1 (make-dir 'D1 empty (list FILE1 FILE2 FILE5)))
(define D2 (make-dir 'D2 (list D1) empty))
(define D3 (make-dir 'D3
                     (list D2)
                     (list FILE3 FILE4 FILE6)))


;; Question 2
#|
;; fs-fun : dir . . . -> . . .
(define (fs-fun a-dir)
   . . . (dir-name a-dir) . . .
   . . . (lod-fun (dir-dirs a-dir)) . . .
   . . . (lof-fun (dir-files a-dir)) . . .)

;; lod-fun : list[dir] . . . -> . . .
(define (lod-fun a-lod)
   (cond [(empty? a-lod) . . . ]
         [(cons? a-lod) . . . (fs-fun (first a-lod)) . . .
                        . . . (lod-fun (rest a-lod)) . . .]
    ))

;; lof-fun : list[file] . . . -> . . .
(define (lof-fun a-lof)
   (cond [(empty? a-lof) . . .]
         [(cons? a-lof) . . . (first a-lof) . . .
                        . . . (lof-fun (rest a-lof)) . . .]
    ))
|#


;; Question 3
;; any-huge-files? : dir number -> boolean
;; consumes a filesystem and a number and returns true if any of the
;;   files in the system are larger than the number
(define (any-huge-files? a-dir a-num)
  (or (lod-huge? (dir-dirs a-dir) a-num)
      (lof-huge? (dir-files a-dir) a-num))
  )

(check-expect (any-huge-files? D0 20) false)
(check-expect (any-huge-files? D1 30) false)
(check-expect (any-huge-files? D2 2) true)
(check-expect (any-huge-files? D3 30) false)

;; lod-huge? : list[dir] number -> boolean
;; consumes a list[dir] and a number and returns true if any of the
;;   files in the subdirectories are larger than the given number
(define (lod-huge? a-lod a-num)
   (cond [(empty? a-lod) false]
         [(cons? a-lod) (or (any-huge-files? (first a-lod) a-num)
                            (lod-huge? (rest a-lod) a-num))]
         ))

;; lof-huge? : list[file] number -> boolean
;; consumes a list[file] and a number and returns true if any of the
;;   files in the list are larger than the given number
(define (lof-huge? a-lof a-num)
   (cond [(empty? a-lof) false]
         [(cons? a-lof) (or (> (file-size (first a-lof)) a-num)
                            (lof-huge? (rest a-lof) a-num))]
         ))

(check-expect (lof-huge? empty 30) false)
(check-expect (lof-huge? (list FILE1 FILE2 FILE3) 30) false)
(check-expect (lof-huge? (list FILE1 FILE2 FILE3) 10) true)


;; Question 4
;; clean-directory : dir symbol -> dir
;; consumes a filesystem and a name and returns that filesystem with
;;   all the files in the named directory of a size 0 deleted
(define (clean-directory a-dir target)
   (cond [(symbol=? (dir-name a-dir) target)
          (make-dir (dir-name a-dir)
                    (dir-dirs a-dir)
                    (filter (lambda (x)
                              (not (= (file-size x) 0)))
                            (dir-files a-dir)))]
         [else (make-dir (dir-name a-dir)
                         (lod-clean (dir-dirs a-dir) target)
                         (dir-files a-dir))]
         ))

(check-expect (clean-directory D0 'D0)
              (make-dir 'D0 empty empty))
(check-expect (clean-directory D1 'D1)
              (make-dir 'D1 empty (list FILE2 FILE5)))
(check-expect (clean-directory D1 'D200)
              (make-dir 'D1 empty (list FILE1 FILE2 FILE5)))
(check-expect (clean-directory D2 'D1)
              (make-dir 'D2
                        (list (make-dir 'D1 empty (list FILE2 FILE5)))
                        empty))

;; lod-clean : list[dir] symbol -> list[dir]
;; consumes a list[dir] and a name and returns the same list with any
;;   files of size zero in the named directory deleted
(define (lod-clean a-lod target)
  (map (lambda (a-dir) (clean-directory a-dir target)) a-lod))


;; Question 5
;; find-file-path : dir symbol -> boolean/list[symbols]
;; consumes a filesystem and a file name (symbol) and returns false if
;;   the file is not in the file system else returns the list of directory
;;   names leading to the file name
(define (find-file-path a-dir nam)
  (cond [(lof-find? (dir-files a-dir) nam) (list (dir-name a-dir))]
        [else (cond [(boolean? (lod-find (dir-dirs a-dir) nam)) false]
                    [(cons? (lod-find (dir-dirs a-dir) nam))
                     (cons (dir-name a-dir)
                           (lod-find (dir-dirs a-dir) nam))])]))

(check-expect (find-file-path D0 'FILE1) false)
(check-expect (find-file-path D1 'FILE1) (list 'D1))
(check-expect (find-file-path D3 'FILE1) (list 'D3 'D2 'D1))
(check-expect (find-file-path D3 'FILE2000) false)

;; lod-find : list[dir] symbol -> boolean/list[symbol]
;; consumes a list[direcotries] and a file name and returns a
;;   list[symbols] representing the path of directories to the given file
;;   or false if the file is not in the directories in the list
(define (lod-find a-lod nam)
  (cond [(empty? a-lod) false]
        [(cons? a-lod) (cond [(cons? (find-file-path (first a-lod) nam))
                              (find-file-path (first a-lod) nam)]
                             [else (lod-find (rest a-lod) nam)]
    )]))

;; lof-find? : list[file] symbol -> boolean
;; consumes a list[file] and a file name and returns true if the given
;;   file is in the list
(define (lof-find? a-lof nam)
  (cond [(empty? a-lof) false]
        [(cons? a-lof) (cond [(symbol=? (file-name (first a-lof)) nam)
                              true]
                             [else (lof-find? (rest a-lof) nam)]
    )]))

(check-expect (lof-find? empty 'FILE1) false)
(check-expect (lof-find? (list FILE1 FILE2 FILE3) 'FILE2) true)
(check-expect (lof-find? (list FILE1 FILE2 FILE3) 'FILE100) false)


;; Question 6
;; file-names-satisfying : dir (file -> boolean) -> list[symbol]
;; consumes a filesystem and a function and returns a list of names for
;;   every file that satisfies the given function
(define (file-names-satisfying a-dir a-fun)
   (append (lod-names (dir-dirs a-dir) a-fun)
           (lof-names (dir-files a-dir) a-fun)))

(check-expect (file-names-satisfying
               D0 (lambda (a-file) (>= (file-size a-file) 10)))
              empty)
(check-expect (file-names-satisfying
               D3 (lambda (a-file) (>= (file-size a-file) 10)))
              (list 'FILE2 'FILE4 'FILE6))
(check-expect (file-names-satisfying
               D1 (lambda (a-file) (>= (file-size a-file) 200)))
              empty)


;; lod-names : list[dir] (file -> boolean) -> list[symbol]
;; consumes a list[dir] and a function and returns a list of the names
;;   of every file in the directories that satisfies the function
(define (lod-names a-lod a-fun)
   (cond [(empty? a-lod) empty]
         [(cons? a-lod) (append (file-names-satisfying (first a-lod)
                                                       a-fun)
                                (lod-names (rest a-lod) a-fun))]
    ))

;; lof-names : list[file] (file -> boolean) -> list[symbol]
;; consumes a list[file] and a function and returns a list of names of
;;   every file in the list that satisfies the function
(define (lof-names a-lof a-fun)
   (map (lambda (a-file) (file-name a-file)) (filter a-fun a-lof)))

(check-expect (lof-names
               empty (lambda (a-file) (>= (file-size a-file) 10)))
              empty)
(check-expect (lof-names
               (list FILE1 FILE2 FILE3)
               (lambda (a-file) (>= (file-size a-file) 10)))
              (list 'FILE2))
(check-expect (lof-names
               (list FILE1 FILE2 FILE3)
               (lambda (a-file) (>= (file-size a-file) 200)))
              empty)


;; Question 7
;; files-containing : dir (file -> elmt -> list[symbol]
;; consumes a filesystem and an element and returns a list of the names
;;   of the files that contain that element
(define (files-containing filesystem elmt)
  (file-names-satisfying
   filesystem
   (lambda (a-file) (equal? elmt (file-content a-file)))))

(check-expect (files-containing D0 "Hello World") empty)
(check-expect (files-containing D1 "Hello World") (list 'FILE2))
(check-expect (files-containing D3 "Hello World") (list 'FILE2 'FILE6))
(check-expect (files-containing D3 "Goodbye?") empty)
(check-expect (files-containing D3 22) empty)