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


;; Type-1
(define (make-not f)
  (cons '- (cons f '())))


; returns x and y with not(-)
;(define (make-not f) 
;  (cond ((not (atom? f)) (car (cdr f))) ; if an atom with negative/a list
;        (else (cons '- (cons f '()))))) ; if an atom




; returns x and y with implies(=>)
(define (make-imply f s)
  (cons f (cons '=> (cons s '() ))))


;-------------fetch--------------;

(define (first-operand clauses)
  (car clauses))

(define (second-operand clauses)
  (caddr clauses))

(define (classifier clauses)
  (cadr clauses))


;; TEST - makes
;(define x 2)
;(define y 4)
;(define z (make-not (make-not 'a))) ; (- a)
;
;(make-and x z)
;
;(make-or x y)
;(make-not y)
;(make-not z)
;(make-imply z y)


; TEST - fetch
(define P 'X)
(define Q 'Y)

;; (-x V Y)
;; -(x ^ -y)

(define input (make-imply P Q))
;; no V and no =>

(define (simplify? input)
  (let ((operator (classifier input))) 
  (cond ((eq? operator 'v) (apply-demorgans input))
        ((eq? operator '=>) (apply-imply input))
        (else #f))))

; demorgans
; get first opp, apply negative
; do same for 2nd opp,
; replce operator with and, then wrap entire list with negation

(define (apply-demorgans input)
    (let ((first-op (first-operand input)) (second-op (second-operand input)))
      (make-not (make-and (make-not first-op) (make-not first-op)))))

(define (apply-imply input)
  (let ((first-op (first-operand input)) (second-op (second-operand input)))
    (make-not (make-and first-op (make-not second-op)))))



(simplify? input)
















;(define P (make-or 'x 'y))
;(define Q (make-and (make-not 'x) 'y))

;(define input (make-imply P Q));
;(display input)


;;;  [x → y] ⇛ (!x v y) 
;
;(define (simp input)
;  (cond ((eq? '=> (classifier input))
;         ((make-or (make-not (first-operand input)) (second-operand input))))
;        
;        (else #f)))
;
;
;(display "\n")
;
;

;
;(display "Display Infos\n")
;
;(first-operand (make-and 'p1 'p2))    ;p1
;(second-operand (make-and 'p1 'p2))   ;p2
;(classifier (make-and 'p1 'p2))       ;^
;
;(make-not 'p1)







;(define (make-and-list lst)
 ; (display lst)
  ; )

;(make-not 3 6)
;(make-not (list '- 'p1) 'p2)
;(make-and-list '(-2 v -2))
