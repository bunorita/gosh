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
