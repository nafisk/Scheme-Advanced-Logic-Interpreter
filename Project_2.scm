(define a (list '-x 'v 'y))

(define (make-and f s)
  (cons f (cons '^ (cons s '() ))))
(define (make-or f s)
  (cons f (cons 'v (cons s '() ))))
(define (make-not f s)
  (cons f (cons '- (cons s '() ))))




(define (first-operand clauses)
  (car clauses))

(define (second-operand clauses)
  (
   
   p1 v p2







(define (make-and-list lst)
  (display lst)
   )

(make-not 3 6)
(make-not (list '- 'p1) 'p2)
;(make-and-list '(-2 v -2))