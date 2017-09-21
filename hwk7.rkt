;; Homework 7 by Lucas Sacherer and Grace Seiche
;; STARTING URL: http://localhost:8088/home

(load "server.rkt")

;; a post is a (make-post string string)
(define-struct post (author body))

;; posts is a global list of posts to display
(define posts empty)

;; home script is the home page
(define-script (home form cookies)
  (values
   (html-page "Forum"
              (append
              (list 'form
                    (list (list 'action "http://localhost:8088/author"))
                    (list 'input (list (list 'type "submit")
                                       (list 'value "Reply"))))
              (format-posts posts)))
   false))

;; author script is the author page
(define-script (author form cookies)
  (values
   (html-page "Author"
              (list 'form
                    (list (list 'action "http://localhost:8088/preview")
                          (list 'target "_blank"))
                    "Author: "
                    (list 'input (list (list 'type "text")
                                       (list 'name "author")))
                    (list 'br)
                    "Body: "
                    (list 'input (list (list 'type "text")
                                       (list 'name "body")))
                    (list 'br)
                    (list 'input (list (list 'type "submit")
                                       (list 'value "Preview")))))
   false))

;; preview script
(define-script (preview form cookies)
  (values
   (html-page "Preview"
              (append (string->xexpr (string-append
                                      "<p>"
                                      (cdr (assoc 'author form))
                                      ": "
                                      (cdr (assoc 'body form))
                                      "</p>"))
                      (list (list 'form
                            (list (list 'action "http://localhost:8088/submit"))
                            (list 'input (list (list 'type "submit")
                                               (list 'value "Accept")))
                            (list 'input (list (list 'type "hidden")
                                               (list 'name "author")
                                               (list
                                                'value
                                                (cdr (assoc 'author form)))))
                            (list 'input (list (list 'type "hidden")
                                               (list 'name "body")
                                               (list
                                                'value
                                                (cdr (assoc 'body form)))))))
                      (list (list 'form
                            (list (list 'action "http://localhost:8088/author"))
                            (list 'input (list (list 'type "submit")
                                               (list 'value "Cancel")))))))
                            false))
              
;; submit script
(define-script (submit form cookies)
  (set! posts (cons (make-post (cdr (assoc 'author form))
                               (cdr (assoc 'body form))) posts))
  (invoke "home" form cookies))


;; format-posts list[string] -> list[string]
(define (format-posts a-los)
  (cond [(empty? a-los) empty]
        [(cons? a-los)
         (append (list (string->xexpr (string-append
                                      "<p>"
                                      (format "~a: ~a" (post-author (first a-los))
                                              (post-body (first a-los)))
                                      "</p>")))
                 (format-posts (rest a-los)))]))
