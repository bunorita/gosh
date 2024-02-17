; #!/opt/homebrew/bin/gosh
; (define (main args)
;     (print (string-join (cdr args) " "))
;     0
; )

; (define (sum-of-numbers lis)
;   (fold + 0 lis))

; (define (product-of-numbers lis)
;   (fold * 1 lis))

; (define (pick-greater a b)
;     (if (> a b) a b))

; (define (max-number lis)
;   (fold pick-greater -inf.0 lis))

; (define (max-number lis)
;   (if (null? lis)
;     (error "max-number needs at least one number")
;     (fold pick-greater (car lis) (cdr lis))))

; (define (print-elements lis)
;   (define (print-first-element a b) (print a))
;   (fold print-first-element #f lis))

(define (fold proc init lis)
  (if (null? lis)
    init
    (fold proc
      #?=(proc (car lis) init)
      #?=(cdr lis))))

(define (last-pair lis)
  (if (pair? (cdr lis))
    (last-pair (cdr lis))
    lis))

(define (copy-list lis)
  (if (pair? lis)
    (cons (car lis) (copy-list (cdr lis)))
    lis))

(define (append2 a b)
  (if (pair? a)
    (cons (car a) (append2 (cdr a) b))
    b))

(define (reverse lis)
  ; (fold cons () lis))
  (if (null? (cdr lis))
    lis
    (append2 (reverse (cdr lis)) (list (car lis)))))

(define (find proc lis)
  (cond
    [(null? lis) #f]
    [(proc (car lis)) (car lis)]
    [else (find proc (cdr lis))]))

; ex 6.5-2
(define (filter proc lis)
  (cond
    [(null? lis) '()]
    [(proc (car lis)) (cons (car lis) (filter proc (cdr lis)))]
    [else (filter proc (cdr lis))]))

; 6.6
; NOT tail recursion
; (define (length lis)
;   (if (null? lis)
;     0
;     (+ 1 (length (cdr lis)))))

; Tail recursion
(define (length lis)
  (define (length-rec lis n)
    (if (null? lis)
      n
      (length-rec (cdr lis) (+ n 1))))
  (length-rec lis 0))

; reverse by tail recursion
(define (reverse lis)
  (define (reverse-rec rest reversed)
    (if (null? rest)
      reversed
      (reverse-rec (cdr rest) (cons (car rest) reversed))))
  (reverse-rec lis ()))
