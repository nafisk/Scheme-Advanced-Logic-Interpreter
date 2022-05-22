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

; returns the negative appropirate to the input
(define (make-not f) 
  (cond ((atom? f) (cons '- (cons f '()))) 
        ((or (eq? (car (cdr f)) '^)
             (eq? (car (cdr f)) 'v)
             (eq? (car (cdr f)) '=>))
         (cons '- (cons f '())))
        ((eq? (car f) '-) (car (cdr f)))))





;-------------fetch--------------;


(define (first-operand clauses)
  (car clauses))

(define (classifier clauses)
  (cond((atom? clauses) clauses)
       (else (cadr clauses))))

(define (second-operand clauses)
  (caddr clauses))





;-------------Operations--------------;

;;  input : (x v y) 
(define (simplify input)
  (let ((operator (classifier input))) 
    (cond ((atom? input) input)
          ((eq? (car input) '-) (make-not (simplify (cadr input))))
          ((eq? operator 'v) (apply-demorgans input))
          ((eq? operator '=>) (apply-imply input))
          (else (make-and (simplify (first-operand input)) (simplify (second-operand input))))))) ;;AND finxed


(define (apply-demorgans input)
 ;; (display input) (display "\n")
    (let ((first-op (first-operand input)) (second-op (second-operand input)))
      (make-not (make-and (make-not (simplify first-op)) (make-not (simplify second-op))))))

 
(define (apply-imply input)
  (let ((first-op (first-operand input)) (second-op (second-operand input)))
    (make-not (make-and (simplify first-op) (make-not (simplify second-op))))))


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Backend----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;


; (define asso-list '((x #f) (y #t)))
(define asso-list '())

(define (lookup input)
  (define (aux lst)
    (cond ((null? lst) (display "Couldn't Found"))
          ((eq? input (caar lst)) (cadr(car lst)))
          (else (aux (cdr lst)))))
    (aux asso-list))



(define (myeval input)
  (cond ((atom? input) (lookup input))
        ((eq? (car input) '-) (not (myeval (cadr input))))
        (else (and (myeval (first-operand input))
                   (myeval (second-operand input))))))




;; INTRODUCTION CALL
(define (evaluate-preposition x y)
  (set! asso-list y)
  (myeval (simplify x))
 )

; test
(evaluate-preposition '((x ^ y) v (x ^ y)) '((x #t) (y #f)) )


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------TESTING----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;


; --- TESTING VARS ---
;(define x 2)
;(define y 4)
;(define z (make-not (make-not 'a)))

;; --- MAKE TEST ---
;(make-and x z)
;(make-or x y)
;(make-not y)
;(make-not '(- x))
;(make-not '(- (- (- x))))
;(make-not '(- (- x)))
;(make-not '(- x))
;(make-or (make-or (make-or (make-not 'x) 'y) (make-or 'x 'y)))
;(make-imply z y)

;; --- HELPER TEST ---
; (remove-negative (make-not (make-not (make-not (make-not (make-not (make-not z)))))))

;; --- MAKE FETCH ---
;(define P 'X)
;(define Q 'Y)
;(define input (make-imply P Q))

;; -- TESTING SIMPLIFY --
;(define gg (make-or (make-or (make-not 'x) 'y) (make-or 'x 'y)))
;(simplify gg)

;; -- EVALUATION --
;; '(x v y) --> (F v t) --> T
;(myeval (simplify '(x v y))) ;;--> COreetly returns T
;(myeval (simplify '(x => y))) ;; correty returns T
;(myeval (simplify '((x v y) v y))) ;; -->correctly T
;(myeval (simplify '((x ^ y) v (x ^ y)))) ;;Correltly returns #f

;; -- INTRODUCTION --
;(evaluate-preposition '(((x ^ y) v (x ^ y)) ((x #t) (y #f))) )