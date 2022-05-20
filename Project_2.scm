;; The City College of New York
;; Computer Science Department
;; CSC 33500 – Programming Language Paradigms
;; Professor Douglas Troeger
;; Project 2 - Scheme List Interpreter
;; by
;; Nafis Khan, email: nkhan014@citymail.cuny.edu
;; Deepankar Chakraborty, email: dchakra001@citymail.cuny.edu

;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Helper-----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

; returns true if element is an atom
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Part 1-----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

;-------------makes--------------;

; returns x and y with and(^)
(define (make-and f s)
  (cons f (cons '^ (cons s '() ))))


; returns x and y with or(v)
(define (make-or f s)
  (cons f (cons 'v (cons s '() ))))

; returns x and y with implies(=>)
(define (make-imply f s)
  (cons f (cons '=> (cons s '() ))))

;; Type-1
(define (make-not f)
  (cons '- (cons f '())))


; returns x and y with not(-)
;(define (make-not-a f) 
;  (cond ((not (atom? f)) (car (cdr f))) ; if an atom with negative/a list
;        (else (cons '- (cons f '()))))) ; if an atom

(define f (cons 'x '()))
(define testLst (make-not (make-not (make-not f))))

(define (remove-negative tree)
  (define (aux lst count)
    ;(display "list: ") (display lst) (display ", count: ") (display count) (display "\n")
        (cond ((null? lst) lst)
              ((atom? lst) (if (= (modulo count 2) 0)
                                                (cons 'x '())
                                                (cons '- (cons 'x '()))
                                                ))
              (else (aux (car (cdr lst)) (if (equal? (car testLst) '-)
                                             (+ count 1)
                                             count))))
    )

  (aux tree 0)
)

; 2nd condition with (x) and not just x
;((not (eq? (car lst) '-)) (if (= (modulo count 2) 0)
;                                                (cons 'x '())
;                                                (cons '- (cons 'x '()))
;                                                ))

;-------------fetch--------------;

(define (first-operand clauses)
  (car clauses))

(define (second-operand clauses)
  (caddr clauses))

(define (classifier clauses)
  (cadr clauses))


;-------------TEST MAKES--------------;
(define x 2)
(define y 4)

(define z (make-not (make-not 'a)))
;(make-and x z)
;(make-or x y)
;(make-not y)
z
(make-not z)
(remove-negative z)

;(make-imply z y)


; TEST - fetch
;(define P 'X)
;(define Q 'Y)

;; (-x V Y)
;; -(x ^ -y)

;(define input (make-imply P Q))
;; no V and no =>


;((x v y) v y)

;(-x v y): --> -(--x ^ -y) 



; plan:
; 1. get the smallest operand
; 2. apply dem or imp
; 3. simplify if there's -- beside each other
; 4. recurse out and then do the bigger one
; look up youtube cideos on how to traverse trees in scheme
; findint he algebraic expressions interpreter whatever in the notes


(define (opp input)
  (cond ((not(atom? (first-operand input))) (apply-laws (first-operand input)))
        ((not(atom? (first-operand input))) (apply-laws (se-operand input)))))


;; -------------

(define (apply-laws input)
  (let ((operator (classifier input))) 
  (cond ((eq? operator 'v) (apply-demorgans input))
        ((eq? operator '=>) (apply-imply input))
        (else input))))


(define (apply-demorgans input)
    (let ((first-op (first-operand input)) (second-op (second-operand input)))
      (make-not (make-and (make-not first-op) (make-not second-op)))))

(define (apply-imply input)
  (let ((first-op (first-operand input)) (second-op (second-operand input)))
    (make-not (make-and first-op (make-not second-op)))))

;(apply-laws (make-or (make-not 'x) 'y))
;(apply-laws (make-or 'x 'y))

(define x (apply-laws (make-or (make-not 'x) 'y)))


;(- (- x))--> #t
;(eq? (car '(- (- x))) (caar (cdr '(- (- x)))))


(define (no-not input)
  (cond ((eq? (car input) (caar (cdr input))) (cadr (cadr input)))))

;(no-not '(- (- x)))





;; -------------------------------
;;;EXAMPLE-1 
;(define P (make-or 'x 'y))
;(define Q 'y)

;; algebraic 

;;(display "Input: ")
;;(define input (make-or P Q));

;;(display input)
;;(display "\n")
;;(apply-laws input)



;
;;; -------------------------------
;;;EXAMPLE-2 
;(define P 'X)
;(define Q 'Y)
;
;(define input (make-imply P Q))
;
;(display input)
;(display "\n")
;(apply-laws input)
;
;;((x #t) (y #f))


;; Backend ----------------------------------------------------
;; (make-not (make-and x (make-not 'y))))
;; ((x #t) (y #f))
;; 
;; -(x ^ -y)
;; -(#t ^ -#f)
;; -(#t ^ #t)
;; -(#t)
;; #f

