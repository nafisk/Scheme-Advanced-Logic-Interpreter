
;(define a (list '-x 'v 'y))

;; p= (x v y)
;; Q = (not x ^ y)
;; p => Q



(define (make-and f s)
  (cons f (cons '^ (cons s '() ))))

(define (make-or f s)
  (cons f (cons 'v (cons s '() ))))

(define (make-not f)
  (cons '- (cons f '())))
; TODO - if ! already present, cancel it out

(define (make-imply f s)
  (cons f (cons '=> (cons s '() ))))


(define P (make-or 'x 'y))
(define Q (make-and (make-not 'x) 'y))

;(make-or 'x 'y)

(define inpt (make-imply P Q))

;;  [x → y] ⇛ (!x v y) 

(define (simp input)
  (cond ((eq? '=> (classifier input))
         ((make-or (make-not (first-operand input)) (second-operand input))))
        
        (else #f)))







(display "\n")


(define (first-operand clauses)
  (car clauses))

(define (second-operand clauses)
  (caddr clauses))

(define (classifier clauses)
  (cadr clauses))

(display "Display Infos\n")

(first-operand (make-and 'p1 'p2))    ;p1
(second-operand (make-and 'p1 'p2))   ;p2
(classifier (make-and 'p1 'p2))       ;^

(make-not 'p1)







;(define (make-and-list lst)
 ; (display lst)
  ; )

;(make-not 3 6)
;(make-not (list '- 'p1) 'p2)
;(make-and-list '(-2 v -2))
