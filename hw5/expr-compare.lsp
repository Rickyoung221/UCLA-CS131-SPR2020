#lang racket
(provide (all-defined-out))

; hint on judging lambda
(define (lambda? x)
(if (or (equal? x 'lambda) (equal? x 'λ)) #t
    #f))

(define (make-binding x y)
	(string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)

(define (checker data x)
    (if (null? data) #f
        (if (equal? (car data) x) #t
            (checker (cdr data) x) )
    )
)

; return a new expression with similar parts combined as a list
; of symbols. Need to include:
; constant literals, variables, procedure calls, quote, lambda, if
(define (expr-compare x y)
    (cond
        [(equal? x y) x]
        ; if one of them is empty, not match
        [(or (null? x) (null? y)) (list 'if '% x y)]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) (not (list? y))) (list 'if '% x y)]
        [else (list-check x y)]        
    )
)


; - If both input use lambda or λ, use the one used in the input.
; - If you do not merge them, like (if % (lambda ...) (λ ...)), leave them as given.
; - If one use lambda and one use λ and you have to merge them into one, use λ.
(define (lambda-compare x y)
  (cond
    [(or (equal? (cdr x) empty) (equal? (cdr y) 'empty)) (list(list 'if '% (car x) (car y)))]
    [(equal? (length (cadr x)) (length (cadr y)))
     (if (or (equal? (car x) 'λ) (equal? (car y) 'λ))
         (cons 'λ (lambda-compare2 empty (cadr x) (cadr y) (caddr x) (caddr y)))
         (cons 'lambda (lambda-compare2 empty (cadr x) (cadr y) (caddr x) (caddr y))))]
      ;otherwise
      [else (list 'if '% x y)]
    ))


(define (lambda-compare2 merge x-i y-i x-j y-j)
   (if (not (null? x-i))
      (if (equal? (car x-i) (car y-i))
           (lambda-compare2 (cons (car x-i) merge) (cdr x-i) (cdr y-i) x-j y-j)
             ;otherwise
             (let ((merged (make-binding (car x-i) (car y-i))) )
                (lambda-compare2 (cons merged merge) (cdr x-i) (cdr y-i)
                                 (lambda-helper (car x-i) merged x-j)
                                 (lambda-helper (car y-i) merged y-j))
             ))
      ; otherwise 
     (cons (reverse merge) (cons (expr-compare x-j y-j) empty)))
)

(define (fnx exp)
  (cond ((and (list? exp)
              (eq? (car exp 'lambda)
                   (atom? (cadr exp)))
              ('lambda 'x (subst (cddr exp) (cadr exp) 'x)))
         (else (error "argument is not a lambda")))))







         
(define (lambda-helper prev merged data)
    (if (symbol? data)
        (if (equal? prev data) merged
            data
        )
        (if (list? data)
            (if (null? data) empty
               (let ( (hd (car data))  )
                   (cond
                        ;;three cases
                        [(equal? hd 'quote) data]
                        ; note that the list for map need to be the same length
                        [(equal? hd 'if) (cons 'if (map (lambda (temp-data) (lambda-helper prev merged temp-data)) (cdr data)))]
                        [(lambda? hd)  (if (checker (cadr data) prev) data
                                (list (car data) (cadr data) (lambda-helper prev merged (caddr data)))        
                                )]
                        [else (map (lambda (temp-data) (lambda-helper prev merged temp-data)) data)]
                    )))
            data
)))

(define (if-compare x y)
  (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y))))

(define (list-check x y)
   (cond
     ; If the list length are different, they must not match
     [(not (= (length x) (length y))) (list 'if '% x y)]
     ;; QUOTE
     ; if any of them or both are quotes, not match
     [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
     ;; LAMBDA
     ; if both are lambda functions
     [(and (lambda? (car x)) (lambda? (car y))) (lambda-compare x y)]
     ;; IF
     ; if both are if statements
     [(and (equal? (car x) 'if) (equal? (car y) 'if)) (if-compare x y)]
     [else (list-compare x y)]
    )
)

(define (list-compare x y)
    (if (not(null? x)) 
        (cons (expr-compare (car x) (car y)) (list-compare (cdr x) (cdr y)))
        x
))



; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

; WARNING: IT MUST BE A SINGLE TEST CASE
; You need to cover all grammars including:
;     constant literals, variables, procedure calls, quote, lambda, if
(define test-expr-x
  (list
   '(cons a b)
   '(cons c b)
   (+ 12 20)
    (quote (12 20))
    (if #t 1 2)
    '(lambda (lambda) lambda)
    '((lambda (3) 3) 3))
  )
  

(define test-expr-y
  (list
   '(cons a b)
   '(cons a b)
   (* 12 20)
    (quote (* 1 2))
    (if #f 1 2)
    '(λ (λ) λ)
    ''((λ (1) 1) 2) 
  )
  )

(expr-compare 12 12)
(expr-compare 12 20)
(expr-compare #t #t)
(expr-compare #f #f)
(expr-compare #t #f)
(expr-compare #f #t)
(expr-compare 'a '(cons a b))
(expr-compare '(cons a b) '(cons a b))
(expr-compare '(cons a lambda) '(cons a λ))
(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(cons a b) '(list a b))
(expr-compare '(list) '(list a))
(expr-compare ''(a b) ''(a c))
(expr-compare '(quote (a b)) '(quote (a c)))
(expr-compare '(quoth (a b)) '(quoth (a c)))
(expr-compare '(if x y z) '(if x z z))
(expr-compare '(if x y z) '(g x y z))
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
(expr-compare '(cons a lambda) '(cons a λ))
(expr-compare '(lambda (a) a) '(lambda (b) b))
(expr-compare '(lambda (a) b) '(cons (c) b))
(expr-compare '((lambda (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
(expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
(expr-compare ''lambda '(quote λ))
(expr-compare '(lambda (a b) a) '(λ (b) b))
(expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
(expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
(expr-compare '(λ (x) ((λ (x) x) x))
              '(λ (y) ((λ (x) y) x)))
(expr-compare '(((λ (g)
                   ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
                    (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
                 (λ (r)                               ; Here (r) will be the function itself
                   (λ (n) (if (= n 0)
                              1
                              (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
                10)
              '(((λ (x)
                   ((λ (n) (x (λ () (n n))))
                    (λ (r) (x (λ () (r r))))))
                 (λ (g)
                   (λ (x) (if (= x 0)
                              1
                              (* x ((g) (- x 1)))))))
                9))

(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))

(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))

