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

;  input : (x v y)

(define (simplify-propositions proposition)

  (define (simplify input)
    (let ((operator (classifier input))) 
      (cond ((atom? input) input)
            ((eq? (car input) '-) (make-not (simplify (cadr input))))
            ((eq? operator 'v)    (apply-demorgans (simplify (first-operand input)) (simplify (second-operand input))))
            ((eq? operator '=>)   (apply-imply     (simplify (first-operand input)) (simplify (second-operand input))))
            (else                 (make-and (simplify (first-operand input)) (simplify (second-operand input)))))))



  ; Cdring down the list... return atokm, empty?
  ;; Terminate

  (define (apply-demorgans x y)
    (make-not (make-and (make-not x)
                        (make-not y))))

 
  (define (apply-imply x y)
    (make-not (make-and x
                        (make-not y))))
  
  (simplify proposition))

;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Backend----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

(define (evaluator simp-prop asso-list)

  (define (lookup input)
    (define (aux lst)
      (cond ((null? lst) '())
            ((eq? input (caar lst)) (cadr(car lst)))
            (else (aux (cdr lst)))))
    (aux asso-list))



  (define (myeval simp-prop)
    (cond ((atom? simp-prop) (lookup simp-prop))
          ((eq? (car simp-prop) '-) (not (myeval (cadr simp-prop))))
          (else (and (myeval (first-operand simp-prop))
                     (myeval (second-operand simp-prop))))))

(myeval simp-prop))


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Part-3 Testing ----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

(define (interpreter input-prop asso-list)
  (evaluator (simplify-propositions input-prop)
             asso-list))

;; Ex Test:

(interpreter '(x v y)       '((x #f) (y #t) (z #t)))
(interpreter '(x => y)      '((x #f) (y #t) (z #t)))
(interpreter '((x v y) v y) '((x #f) (y #t) (z #t)))
(interpreter '((x ^ y) v (x ^ y)) '((x #f) (y #t) (z #t)))
(interpreter '((- (x ^ y)) ^ z)   '((x #f) (y #t) (z #t)))
(interpreter '(x ^ y)       '((x #f) (y #t) ))
(interpreter '((- x) ^ ((x ^ (x v y)) v (y v x))) '((x #f) (y #t) ))
(interpreter '(- x) '((x #f)))

;(myeval (simplify '(x v y))) ;;--> COreetly returns T
;(myeval (simplify '(x => y))) ;; correty returns T
;(myeval (simplify '((x v y) v y))) ;; -->correctly T
;(myeval (simplify '((x ^ y) v (x ^ y)))) ;;Correltly returns #f
;(myeval '((- (x ^ y)) ^ z))  























;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------GARBAGE----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;
;; (make-not (make-and x (make-not 'y))))
;; ((x #t) (y #f))
;; 
;; -(x ^ -y)
;; -(#t ^ -#f)
;; -(#t ^ #t)
;; -(#t)
;; #f

;;;;;; INPUT??? ;;;;;;;;;
;(define truth-vals '())
;; a: (x ^ y)
;; b: ((x t) (y f))
;(define (calc-result a b)
;  (append b truth-vals)
;  (simplify a)
;  )

;(calc-result '(x ^ y) '((x #t) (y #f)))


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



;; EXTRA STUFF


;; PLEASE CHANGE NAME 🛑🛑🛑🛑🛑
;(define (simp-alt input)
;  (cond ((null? input) '())
;        ((atom? input) input)
;        (else (append (simplify input)))))
;
;
;
;

