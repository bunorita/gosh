; #!/opt/homebrew/bin/gosh
; (define (main args)
;     (print (string-join (cdr args) " "))
;     0
; )

(define (sum-of-numbers lis)
  (fold + 0 lis))

(define (product-of-numbers lis)
  (fold * 1 lis))

; (define (pick-greater a b)
;     (if (> a b) a b))

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


; 7.1
(define (max-number lis)
  (if (null? lis)
    (error "max-number needs at least one number")
    (fold
      (lambda (a b) (if (> a b) a b))
      (car lis)
      (cdr lis))))

(define (print-elements lis)
  (fold
    (lambda (a b) (print a))
    #f
    lis))

; 7.2
(define (print-elements lis)
  (for-each (lambda (a) (print a)) lis))

; (for-each proc lis)
(for-each (lambda (x) (print "> " x)) '(a b c))

; (walker proc lis)
; walker はフラットなリストに作用する手続き
(define (tree-walk walker proc tree)
  (walker
    (lambda (elt)
      (if (list? elt)
        (tree-walk walker proc elt)
        (proc elt)))
    tree))

(tree-walk for-each print
  '((1 2 3) 4 5 (6 (7 8))))

(define (reverse-for-each proc lis)
  (for-each proc (reverse lis)))

(tree-walk reverse-for-each print
  '((1 2 3) 4 5 (6 (7 8))))

; (map proc lis)
(map (lambda (x) (* x 2)) '(1 2 3 4))

(tree-walk map (lambda (x) (* x 2))
  '((1 2 3) 4 5 (6 (7 8))))

(define (reverse-map proc lis)
  (map proc (reverse lis)))

(tree-walk reverse-map (lambda (x) x)
  '((1 2 3) 4 5 (6 (7 8))))

(define (reversed walker)
  (lambda (proc lis)
    (walker proc (reverse lis))))

(define reverse-for-each (reversed for-each))
(define reverse-map (reversed map))

; ex7.2-1 for-each-numbers
(define (for-each-numbers proc lis)
  (for-each proc (filter number? lis)))

; ex7.2-2 map-numbers
(define (map-numbers proc lis)
  (map proc (filter number? lis)))

; ex7.2-3 number-only
(define (number-only walker)
  (lambda (proc lis)
    (walker proc (filter number? lis))))

; identity function
(define (idf x) x)
; (map idf '(1 2 (3)))
; > (1 2 (3))
; (map-numbers idf '(1 2 (3)))
; > (1 2) // want (1 2 (3))
; define numbers-only-for-tree
; (tree-walk (numbers-only-for-tree map|for-each) proc tree)
(define (filter-number-for-tree tree)
  (if (null? tree)
    '()
    (if (list? (car tree))
      (cons (filter-number-for-tree (car tree)) (filter-number-for-tree (cdr tree)))
      (if (number? (car tree))
        (cons (car tree) (filter-number-for-tree (cdr tree)))
        (filter-number-for-tree (cdr tree))))))

(define (numbers-only-for-tree walker)
  (lambda (proc lis)
    (walker proc (filter-number-for-tree lis))))

; for-each, map, fold... can take multiple lists as arguments
(for-each
  (lambda (x y) (print x " -> " y))
  '(a b c) '(1 2 3))
(map list '(1 2 3) '(4 5 6) '(7 8 9))

; 7.3 local variables
((lambda (a b) (+ (* a a) (* b b))) 3 4)
; can be written as below.
(let
  ((a 3) (b 4))
  (+ (* a a) (* b b)))

; let expressions can nest.
(let ((a 3)
      (b 4))
  (let ((a b)
        (b a))
    (cons a b)))
; > (4 . 3)

(letrec ((a 1)
         (b (lambda (x) (+ a x))))
  (b 4))
; > 5

(letrec ((sum (lambda (lis)
                 (cond [(null? lis) 0]
                       [(number? (car lis)) (+ (car lis) (sum (cdr lis)))]
                       [else (sum (cdr lis))]))))
(sum '(1 3 #f 6 #t 9)))
; > 19

; 7.4 take variadic arguments
(define (func  a b . c)
  (print "a=" a ", b=" b ", c=" c))
(define func
  (lambda (a b . c) (print "a=" a ", b=" b ", c=" c)))

(define (func . a) (print "a=" a))
(define func
  (lambda a (print "a=" a)))

; ex 7.4-1 define list
(define (list . x) x)
(define list (lambda x x))

; 7.5 pass variadic arguments
(define (append/log . args)
  (print "args=" args)
  (apply append args))

(define (make-logger func)
  (lambda args
    (print "args=" args)
    (apply func args)))

; 7.6 arguments pattern matching
(define (append . args)
  (cond [(null? args) ()]
        [(null? (cdr args)) (car args)]
        [else (append2 (car args) (apply append (cdr args)))]))

(use util.match)
(define (append . args)
  (match args
    [() ()]
    [(a) a]
    [(a . b) (append2 a (apply append b))]))

; max-nuber by arguments pattern matching
(define (max-number lis)
  (match lis
    [() (error "max-number needs at least one number")]
    [(x . y) (fold (lambda (a b) (if (> a b) a b)) x y)]))

; 7.7 optional arguments, keyword arguments
(define (make-list num . args)
  (define (maker n init)
    (if (= n 0)
      ()
      (cons init (maker (- n 1) init))))
  (maker num (if (null? args) #f (car args))))
; make-list by let-optionals
(define (make-list num . args)
  (let-optionals* args ((init #f))
    (define (maker n)
      (if (= n 0)
        ()
        (cons init (maker (- n 1)))))
    (maker num)))

(define (person . args)
  (let-keywords args ((name "Anonymous")
                      (age "unknown")
                      . other-args)
    (print name " is " age " year(s) old.")
    (print "Other info:" other-args)))

; 7.9 multiple return values
; 7.9.1 receive
(min&max 3 1 2) ;> 1 3
(receive (min-val max-val)
  (min&max 3 1 2)
  (list min-val max-val))
; > (1 3)
(receive (min-val . rest)
  (min&max 3 1 2)
  (list min-val rest))
; > (1 .(3))
(receive values
  (min&max 3 1 2)
  values)
; > (1 3)
(let-values (((min-val max-val) (min&max 3 1 2)))
  (format #t "max: ~s\nmin: ~s\n" max-val min-val))


; 8.2
; 8.2.1 eauality
(equal? '(1 2) '(1 . (2))) ;> #t
; 8.2.2 identity
(eq? (cons 1 2) (cons 1 2)) ;> #f
(eq? 'xyz 'xyz) ;> #t
; 8.2.3
(equal? 1 1.0)
(= 1 1.0)
(char=? #\a #\a)
(string=? "abc" "abc")
; ci: case insensitive
(char-ci=? #\a #\A)
(string-ci=? "abc" "ABC")
; 8.2.4
(use srfi-1)
(lset= eqv? '(1 2 3) '(3 1 2)) ; set equality

; 8.3 procedures that handle boolean values
; 8.3.3 procedures that take predicates
(define positive-integer? (every-pred integer? positive?))
(define nonnegative (complement negative?))
; implement on my own
(define (complement pred)
  (lambda (x) (not (pred x))))
; TODO: any-pred, every-pred

; 8.4 condition
; cond
(define lis '(2 4 6 7 8 10))
(cond [(find odd? lis) => (lambda (val) val)]
      [else 0])
(cond [(find odd? lis) => (cut <>)]
      [else 0])
; case
(define (score card)
  (case card
    ((A) 11)
    ((J Q K) 10)
    ((2 3 4 5 6 7 8 9) card)
    (else 0)))
; when, unless
(when #t
  (print "foo")
  (print "bar"))
(unless #f
  (print "foo")
  (print "bar"))

; 9 state
; 9.1 set
; compare by equal?
(define (member elt lis)
  (cond [(null? lis) #f]
        [(equal? elt (car lis)) lis]
        [else (member elt (cdr lis))]))
; passing cmp-fn by optional arg
(define (member elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
    (cond [(null? lis) #f]
        [(cmp-fn elt (car lis)) lis]
        [else (member elt (cdr lis) cmp-fn)])))
(define (member2 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
    (define (loop lis)
      (cond [(null? lis) #f]
        [(cmp-fn elt (car lis)) lis]
        [else (loop (cdr lis))]))
    (loop lis)))
; delete one element
(define (delete-1 elt lis)
  (cond [(null? lis) '()]
        [(equal? elt (car lis)) (cdr lis)]
        [else (cons (car lis) (delete-1 elt (cdr lis)))]))
(define (delete-1 elt lis)
  (define (loop lis)
    (cond [(null? lis) '()]
          [(equal? elt (car lis)) (cdr lis)]
          [else (cons (car lis) (loop (cdr lis)))]))
  (loop lis))
(define (delete-1 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
    (define (loop lis)
      (cond [(null? lis) '()]
            [(cmp-fn elt (car lis)) (cdr lis)]
            [else (cons (car lis) (loop (cdr lis)))]))
    (loop lis)))

; TODO:
; (use gauche.test)
; (let ((data '(1 2 3 4 5)))
;   (test* "non-copy delete-1" data (delete-1 6 data) eq?))

; 9.2 association list
(assoc 'foo '((bar . 3) (foo . 2) (baz . 1)))
;> (foo . 2)
(define (assoc key alist . options)
  (let-optionals* options ((cmp-fn equal?))
    (define (loop alis)
      (cond [(null? alis) #f]
            [(cmp-fn key (caar alis)) (car alis)]
            [else (loop (cdr alis))]))
    (loop alist)))

; 9.3 abstraction
(define (traverse fallback get-key return repeat)
  (lambda (elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
      (define (loop lis)
        (cond [(null? lis) fallback]
              [(cmp-fn elt (get-key lis)) (return lis)]
              [else (repeat loop lis)]))
      (loop lis))))
; define by traverse
(define assoc
  (traverse #f caar car
            (lambda (loop lis) (loop (cdr lis)))))
; 9.4 association list again
(define *item-database*
  '((potion (drink . #t) (throw . #t))
    (elixir (drink . #t) (throw . #t))
    (pancake (eat . #t) (throw . #t))
    (cookie (eat . #t) (throw . #t))
    (dagger (throw . #t))))

(define (item-properties item)
  (cond [(assoc item *item-database*) => cdr]
        [else ()]))

(define (item-property-get item prop)
  (cond [(assoc prop (item-properties item)) => cdr]
        [else #f]))

; 9.5 named-let
'((hp . 320)
 (mp . 66)
 (position . #f)
 (inventory potion potion dagger cookie dagger)
)

(use util.match)
(define (make-player . args)
  (define (loop lis)
    (match lis
      [() '()]
      [(attr value . rest) (cons (cons attr value) (loop rest))]))
  (loop args))
(make-player 'hp 320 'mp 66 'position #f 'inventory '(potion potion dagger cookie dagger))

(define (make-player2 . args)
  (let loop ([lis args])
    (match lis
      [() ()]
      [(attr value . rest) (cons (cons attr value) (loop rest))]
      [_ (error "Number of arguments must be even:" args)])))
(make-player2 'hp 320 'mp 66 'position #f 'inventory '(potion potion dagger cookie dagger))

(define *player*
  (make-player2 'hp 320 'mp 66 'position #f 'inventory '(potion potion dagger cookie dagger)))
