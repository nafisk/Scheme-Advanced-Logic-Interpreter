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
          ;((not (= (length input) 3)) "") ;(display "Incorrect input. Required: <Operand Operator Operand>\n")
          ((eq? operator 'v) (apply-demorgans input))
          ((eq? operator '=>) (apply-imply input))
          (else input))))



;; 🛑🛑🛑🛑------- FIX ^^ FOR AND -------🛑🛑🛑🛑 ;;









(define (apply-demorgans input)
    (let ((first-op (first-operand input)) (second-op (second-operand input)))
      (make-not (make-and (make-not (simp-alt first-op)) (make-not (simp-alt second-op))))))

(define (apply-imply input)
  (let ((first-op (first-operand input)) (second-op (second-operand input)))
    (make-not (make-and first-op (make-not second-op)))))





;; PLEASE CHANGE NAME 🛑🛑🛑🛑🛑
(define (simp-alt input)
  (cond ((null? input) '())
        ((atom? input) input)
        (else (append (simplify input)))))




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








;; Backend ----------------------------------------------------
;; (make-not (make-and x (make-not 'y))))
;; ((x #t) (y #f))
;; 
;; -(x ^ -y)
;; -(#t ^ -#f)
;; -(#t ^ #t)
;; -(#t)
;; #f

