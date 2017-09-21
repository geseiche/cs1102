;; Homework 6 by Lucas Sacherer and Grace Seiche

;; Question 1

(define-syntax class
  (syntax-rules ()
    [(class (initvars vars ...)
       (method name (args ...) fun)...)
     (lambda (vars ...)
       (lambda (message)
         (cond [(symbol=? message 'name)
                (lambda (args ...) fun)]
               ...
               [else (error
                      (format "Function ~a is not defined!" message))])))]))

(define-syntax send
  (syntax-rules ()
    [(send class name vars ...)
     ((class 'name) vars ...)]))

;; Example:

(define dillo-class
  (class (initvars length dead?)
         (method longer-than? (len) (> length len))
         (method run-over () (dillo-class (+ length 1) true))))

(define d3 (dillo-class 5 false))
(send d3 longer-than? 6)
(send d3 longer-than? 5)
(define d4 (send d3 run-over))
(send d4 longer-than? 5)


;; Question 2

(define-syntax policy-checker
  (syntax-rules ()
    [(policy-checker
      (name (action ...) (obj ...))...)
     (let ([positions (list 'name ...)]
           [actions (append (list 'action ...) ...)]
           [objs (append (list 'obj ...) ...)])
     (lambda (a-name an-action an-obj)
       (begin
         (cond [(boolean? (member a-name positions))
                (error (format "~a is not a defined position!" a-name))]
               [(boolean? (member an-action actions))
                (error (format "~a is not a defined action!" an-action))]
               [(boolean? (member an-obj objs))
                (error (format "~a is not a defined object!" an-obj))])
         (cond [(symbol=? a-name 'name)
                (and (not (boolean? (member an-action (list 'action ...))))
                     (not (boolean? (member an-obj (list 'obj ...)))))]
               ...))))]))

;; Example:

(define check-policy
  (policy-checker
   (programmer (read write) (code documentation))
   (tester (read) (code))
   (tester (write) (documentation))
   (manager (read write) (reports))
   (manager (read) (documentation))
   (ceo (read write) (code documentation reports))))

(check-policy 'programmer 'write 'code)
(check-policy 'programmer 'write 'reports)