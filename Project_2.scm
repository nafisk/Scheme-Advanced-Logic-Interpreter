;; The City College of New York
;; Computer Science Department
;; CSC 33500 – Programming Language Paradigms
;; Professor Douglas Troeger
;; Project 2 - Scheme List Interpreter
;; by
;; Nafis Khan, email: nkhan014@citymail.cuny.edu
;; Deepankar Chakraborty, email: dchakra001@citymail.cuny.edu

;; Introduction:
; The Scheme Interprepter Package consists of one way of handling propositional logic using Scheme's functions
; programming logic, syntax and primitives. It extensively uses lists to handle propositional expressions that contains
; not only the expressions that needs to be simplifed to only v and => expressions, but also the boolean evaluation values
; for applying to these expressions after being simplified.
;
; All functions are working completely and is upto spec with the given prompt. This package is able to handle any size of
; correct propositional input and the input works with both concrete and abstract representation. Tests are done with a
; mix of concrete and abstract inputs.

; As a walk through, if there is an input (-x v y), it gets simplified to -(--x ^ -y), then -(-x ^ y) and then evaluated
; with the given association list of ((x #t) (y #f)) in the following steps
; -(x ^ -y)
; -(#t ^ -#f)
; -(#t ^ #t)
; -(#t)
; #f


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Helper-----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;





; Pre-Condition: Given an input x that is either a singular element or a list with elements
; Post-Condition: Returns true if x is a non-null element and false if it is a list.
;
; The atom? function checks if an input given when calling atom? is neither not a null value or not a list. In other words,
; it checks to see if it is a singular value or element. The two functions inside, null? (returns #t for null) And
; pair?(returns #t for lists) checks for this exactly and returns true if both are true and and false if both are false.

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; Tests:
; (atom? 'a)
; Correct Answer: True
;
; (atom? '(a))
; Correct Answer: False





;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Part 1-----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given two components f and s that can be later evaluated using boolean values.
; Post-Condition: Returns a proposition in the form of a list containing f, ‘^’, and s.
;
; The make-and function is used to create a list, representing a proposition with infix notation by initially placing
; the value of the ending component s in an empty cons list, pushing ^ before it by wrapping the initial cons with another
; cons function, followed by the last cons wrapper containing f. This results in the postcondition (f ^ s).

(define (make-and f s)
  (cons f (cons '^ (cons s '() ))))

; Tests:
; (make-and 'a 'b)
; Correct Answer: (a ^ b)
;
; (make-and '(x ^ y) 'z)
; Correct Answer: ((x ^ y) ^ z)




;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given two components f and s that can be later evaluated using boolean values.
; Post-Condition: Returns a proposition in the form of a list containing f, ‘v’, and s.
;
; The make-and function is used to create a list, representing a proposition with infix notation by initially placing the
; value of the ending component s in an empty cons list, pushing ‘v’ before it by wrapping the initial cons with another
; cons function, followed by the last cons wrapper containing f. This results in the postcondition (f v s).


(define (make-or f s)
  (cons f (cons 'v (cons s '() ))))

; Tests:
; (make-or 'a 'b)
; Correct Answer: (a ^ b)
; (make-or '(x ^ y) 'y)
; Correct Answer: ((x ^ y) v y)

;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given two components f and s that can be later evaluated using boolean values.
; Post-Condition: Returns a proposition in the form of a list containing f, ‘=>’, and s.
;
; The make-and function is used to create a list, representing a proposition with infix notation by initially placing the
; value of the ending component s in an empty cons list, pushing ‘=>’ before it by wrapping the initial cons with another
; cons function, followed by the last cons wrapper containing f representing the value. This results in the postcondition
; (f v s).

(define (make-imply f s)
  (cons f (cons '=> (cons s '() ))))

; Tests:
; (make-imply 'a 'b)
; Correct Answer: (a => b)
; (make-imply '(x ^ y) 'y)
; Correct Answer: ((x ^ y) => y)

;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given either f representing a component or a list containing a negative component.
; Post-Condition: Returns either a positive or a negative component.
;
; The make-not function is used to apply negations on given components. By the design of this project, positive components
; are kept as atoms [i.e. x], and negative components are kept as lists [i.e. (- x)] with the first element in the list
; being a negative symbol and the second one being the component itself. If the component is positive, it cons a negative
; ‘- symbol before the component and returns it. If the component is negative, we know that negative times negative is
; positive, therefore, we simply return the component as an atom without its negative symbol. 
; There is also another case that make-not deals with. From the understanding of the project specs, simplification is done
; on the proposition using De-Morgrans law or Lmplication law. Therefore a negative symbol needs to stay outside the entire
; proposition after applying the laws(i.e. -(x ^ -y)) without it getting multiplied into the proposition or for not getting
; any other sorts of errors. Therefore a check is done for a proposition and if the given f, is indeed a proposition, a
; list with a negative of the entire proposition is returned. 

(define (make-not f) 
  (cond ((atom? f) (cons '- (cons f '()))) 
        ((or (eq? (car (cdr f)) '^)
             (eq? (car (cdr f)) 'v)
             (eq? (car (cdr f)) '=>))
         (cons '- (cons f '())))
        ((eq? (car f) '-) (car (cdr f)))))


; Tests: -> Correct
; (make-not 'y)
; (- y)
; (make-not '(- x))
; x
; (make-not '(- (- (- x))))
; (- (- x))
; (make-not '(- (- x)))
; (- x)
; (make-not '(- x))
; x
; (make-or (make-or (make-not (make-not 'x)) 'y) (make-or 'x 'y))
; ((x v y) v (x v y))




;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given a list representing a proposition 
; Post-Condition: Returns the first component of the proposition.
;
; This procedure simply uses the car primitive to get the first element, which in this case is the first component and return
; that.

(define (first-operand clauses)
  (car clauses))

; Tests:
; (first-operand '(x ^ y))
; Correct: x 
; (first-operand '((x ^ y) v (x => y)))
; Correct: (x ^ y)

;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given a list representing a proposition 
; Post-Condition: Returns the infix notation of proposition.
;
; This procedure, checks for two different things. The first one checks if the clauses variable is an atom for a specific
; implementation for the simplify procedure that can be found later on and the else clause simply returns the second element
; in the list representing the proposition that holds any of the three ^, v or =>.

(define (classifier clauses)
  (cond((atom? clauses) clauses)
       (else (cadr clauses))))

; Tests:
; (classifier '(x ^ y))
; Correct: ^ 
; (classifier '((x ^ y) => (x => y)))
; Correct: =>

;-----------------------------------------------------------------------------------------------------------------------;

; Pre-Condition: Given a list representing a proposition 
; Post-Condition: Returns the second component of the proposition.
;
; This procedure simply uses the car and cdr primitives to get the third element in the list, which in this case is the
; second component, and return that.

(define (second-operand clauses)
  (caddr clauses))

; Tests:
; (second-operand '(x ^ y))
; Correct: x 
; (second-operand '((x ^ y) v (x => y)))
; Correct: (x ^ y)

; --------------------------------------------------------------------------------------------------------------------------------------;

; Let us now look at a structual induction of the interpreter:

; Pre-condition: The precondition for the interpreter is, given a list, containing a proposition which uses and(^), or(v),
; not(-) and implies(=>), and an association-list that contains a list of variables-value pairs with one entry per variable
; for all available variables in the proposition. It is also worthy to note that, the propositions are built using the infix
; notation with one clause on the left of the operator, and one clause on the right of the operator. 
;
; Post Condition: It will return computed the logical truth value of the input proposition using alist values for its variables.
;
; Let is now see the proper components of the input, for which we can induct on. We can say that the class of P_k of propositions
; over the set K of proposition variables is the least class containing T, F, and each variable k ∈ K, which is closed under
; ^, V, -, and =>. 
; So, 
; If, X, Y  ∈ P_k, then
;
; (X ^ Y)  ∈ P_k
; (X v Y)  ∈ P_k
; (- X)  ∈ P_k
; (X => y) ∈ P_k,
;
; Similarly, we can also get more complex components by making each operand a new sub-component. So, for (X v Y) ^ (X v Z), the
; components are X, Y, Z, (X v Y), (X v Z). Note that: ), (, =>, ^ are alone not a proper component as it doesn’t fit our
; definition of P_k above. 
;
; Now, as per the specification of the project, the front-end of the interpreter should return a logically equivalent proposition
; using just ^ and -, and without using or(v),  imply(=>). We can proof using structural induction that any proposition written
; using the operators ^, V, -, => is equivalent to a proposition using using just ^ and - . 
;
; Basis: In the basis step, we see our interpreter returns T,F, and which are  k ∈ K. Since it belongs to our definition, we can
; say basis step is true. 
;
; IH: We assume that our recursive call returns the logical eqivelance proposition, given that we provided all proper components
; to the recursive call. 
;
; IS: In the inductive step, we need to consider all the possible cases for a proposition P. 
; P = (X ^ Y)
; P = (X v Y)
; P = (- R)
; P = (X => Y)
;
; (Case1) 
; P = (X ^ Y) → From IH, we can see, provided a propoper component X, there is a logically equivalent proposition X’, where X’ is
; expressed using only ^ and - operator.
; Using the same logic, provided a propoper component Y, there is a logically equivalent proposition Y’, where Y’ is expressed
; using only ^ and - operator.
; Here we clearly see if X’ and Y’ both propositional components match the specification(i.e. no v/=>), it is clear that X’ ^ Y’
; is indeed the right answer. 
;
; (Case2) 
; P = (X v Y) → From IH, we can see, provided a propoper component X, and Y, there is a logically equivalent proposition X’, Y’,
; where X’ is expressed using only ^ and - operator.
;
; But here we need to do a bit of extra work, namely, we can use demorgan’s law to simplify the v case. So, 
; X’ v Y’ = - ((- X’) ^ (- Y’))
; (Case 3) 
; P = - X→ Similarly, from IH we get an X’ which is expressed using only the (^) and (-) operator. Which we can then return as
; (-  X’), as it is already in its proper condition. 
;
; (Case-4)
; (P= X => Y) From IH, we can see, provided a propoper component X, and Y, we will get a logically equivalent proposition X’, Y’,
; where X’ is expressed using only ^ and - operator.
;
; But here we need to do a bit of extra work, namely, we can use the simplification law to simplify the implication(=>). So, 
; X’ => Y’ = -(X’ ^ (- Y’))
; Here we clearly see if X’ and Y’ both propositional components match the specification(i.e. no v/=>), it is clear that -(X’ ^ (- Y’))
; is indeed the right answer. 


; Pre-Condition: Given a propositional stament
; Post: Returns the a simplified version of the propositional input without v or =>

(define (simplify-propositions proposition)

;  Pre-Condition: Given an input list representing a proposition.
;  Post-Condition: Returns a simplified list without the or(v) or implies(=>) classifier
;
;  The simplify function is used as a routing function to apply the correct laws and onto the given proposition by the user. As given
;  in the prompt, the front end will send a value to the back end that contains no or(v) or implied(=>) classifier. Therefore,
;  demorgans law is applied to the propositions with the v operand and implication law is applied to the proposition with the => operand.
;  In the case of the and(^) and negative(-) operands, no specific laws are applied since those already meet the design spec and
;  therefore only make-not and make-negative functions are used due to the operand it has.  
  
  (define (simplify input)
    (let ((operator (classifier input))) 
      (cond ((atom? input) input)
            ((eq? (car input) '-) (make-not (simplify (cadr input))))
            ((eq? operator 'v)    (apply-demorgans (simplify (first-operand input)) (simplify (second-operand input))))
            ((eq? operator '=>)   (apply-imply     (simplify (first-operand input)) (simplify (second-operand input))))
            (else                 (make-and (simplify (first-operand input)) (simplify (second-operand input)))))))

  
;-----------------------------------------------------------------------------------------------------------------------;

;  Pre-cond: Given two proper components of X, Y, where X,Y ∈ P_k,
;  Post-condition: Returns the proposition by applying the demorgan's law. 
;
;  The function takes two proper components, negates x and y, adds an infix and(^) notation, and then negates the entire proposition
;  by using the previously proven make-and and make-not functions and returning the correct proposition created by using the
;  demorgans law.
  
  (define (apply-demorgans x y)
    (make-not (make-and (make-not x)
                        (make-not y))))

;-----------------------------------------------------------------------------------------------------------------------;
  
;  Pre-cond: Given two proper components of X, Y, where X,Y ∈ P_k,
;  Post-condition: Returns the proposition by applying the implication law for simplification. 
;
;  This function takes is used to apply the implication law to the given x and y components. According to the implication rule, in this
;  case, Y is negated using make-not, and a proposition is created with this negative y and given x. The entire proposition is further
;  wrapped by a negative to convert the entire proposition negative and then returned.

  (define (apply-imply x y)
    (make-not (make-and x
                        (make-not y))))
  (simplify proposition)


  )


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Backend----------------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;

; Pre-condition: Given a proposition, which has already been simplified using the front-end(Part-1), that has only and(^)
; and not(-) as its operator in infix notation.  
; Post-Condition: Returns the computed value of the input proposition using those values for its variables. 
;
; The Evaluator is a wrapper function that calls two more functions to compute the input proposition. 

(define (evaluator simp-prop asso-list)

  ; Pre-condition: Given an look-up variables that exist on the simplified proposition, 
  ; Post-condition: Returns the truth value of the look-up variable from the association-list that contains a list of variable-value
  ; pairs with one entry per variable.  
  ; Ex: (alist = ((x #t) (x #f) (z #f)) ) 
  ;
  ; We can write an iterative function for this problem. A design idea for an iterative program is to consider the list in two parts,
  ; the car and the cdr of the list. In the iterative process, we will extract the caar of the list, and compare it with our lookup value,
  ; and if found return the appropriate truth value. Otherwise, we will cdr down the list by passing the cdr of the list to the next call. 
  ; gI: Let us consider a variable, ASSO-LIST, which represents the original input of our function, and thus not changing. 
  ; The gI holds, if input ∈ ASSO-LIST, implies that, <=⇒ input also ∈ asso-list, where asso-list is the formal parameter
  ;
  ; Weak Enough?: Since, list input == ASSO-LIST on start, the gI becomes, input ∈ ASSO-LIST if and only if, input  ∈ ASSO-LIST. 
  ; Strong Enough?: Let us consider the termination condition of the function. The function can terminate, 
  ; (null? asso-list) means we are given an empty list, which implies that, since, input ∉ ASSO-LIST , we know that p can’t belong to list s,
  ; input  ∉ asso-list, and it is correct to return #F. On the other hand, if input belongs to the cdr of the asso-list,  input ∈ (car asso-list),
  ; then a also belongs to ASSO-LIST, input ∈ ASSO-LIST, which is the list at the beginning of the call, and the function will stop and correctly return #T.   
  ; Preservable?: In ord
  
  (define (lookup input)
    (define (aux lst)
      (cond ((null? lst) '())
            ((eq? input (caar lst)) (cadr(car lst)))
            (else (aux (cdr lst)))))
    (aux asso-list))

;-----------------------------------------------------------------------------------------------------------------------;

  ; Pre-condition:  Given a proposition, which has already been simplified using the front-end(Part-1), that has only and(^) and not(-) as its operator
  ; in infix notation.
  ; Post-Condition: it returns the truth value of the simplified-proposition or the original input proposition. 
  ;
  ; Let us build a divide and conquer strategy for this function. We will be inducting on the length of the simp-prop list and its variables. 
  ;
  ; Basis Step: We stop recursion when we reach the inner variable of simp-prop, which is an atom. Once we reach the variable in the simp-prop list,
  ; we can return the truth value of the variable using the lookup function. 
  ;
  ; IH: It can be seen in two ways,
  ; Given that our pre-condition satisfies, that means the, simp-prop is a proper component of P_k, We assume that the call (myeval (first-operand simp-prop))
  ; returns the truth value of the first-operand of the input proposition, (first-operand simp-prop),  by evaluating all the variables that exist in the
  ; operand by applying logical(scheme primitive) AND and NOT operation. Same argument applies for the second-operand of the simp-prop proposition. 
  ; Also, (- P) is also a proper component of the P_k, so, given we have a not in our component, the function call, (myeval (cadr simp-prop)), returns
  ; the truth value of the component P,  by evaluating all the variables that exist in the proposition. 
  ;
  ; IS:  We can see that the precondition satisfies, since return the truth value smallest unit of the proposition: (atom? simp-prop) (lookup simp-prop). 
  ; So, the code handles the result of the recursive call in 2 ways, it either applies primitive NOT to the result of the recursive call, (myeval (cadr simp-prop)),
  ; or, it applies the primitive AND to the result of the recursive call to the first and second operand. Thus, computing the truth value of the input proposition. 

  (define (myeval simp-prop)
    (cond ((atom? simp-prop) (lookup simp-prop))
          ((eq? (car simp-prop) '-) (not (myeval (cadr simp-prop))))
          (else (and (myeval (first-operand simp-prop))
                     (myeval (second-operand simp-prop))))))

(myeval simp-prop)
)


;-----------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------------Part-3 Testing --------------------------------------------------;
;-----------------------------------------------------------------------------------------------------------------------;



(define (interpreter input-prop asso-list)
  (evaluator (simplify-propositions input-prop)
             asso-list))

;; Tests:
;(interpreter '(x v y)       '((x #f) (y #t) (z #t))) ; #t
;(interpreter '(x => y)      '((x #f) (y #t) (z #t))) ; #t
;(interpreter '((x v y) v y) '((x #f) (y #t) (z #t))) ; #t
;(interpreter '((x ^ y) v (x ^ y)) '((x #f) (y #t) (z #t))) ; #f
;(interpreter '((- (x ^ y)) ^ z)   '((x #f) (y #t) (z #t))) ; #t
;(interpreter '(x ^ y)       '((x #f) (y #t) )) ; #f
;(interpreter '((- x) ^ ((x ^ (x v y)) v (y v x))) '((x #f) (y #t) )) ; #t
;(interpreter '(- x) '((x #f))) ; #t

