;Ganimet Yigit
;112200078
;PROJECT6-CMPE314
#lang racket
(require plai-typed)
(print-only-errors #f)

; Alphabet {+,-,*,**,id, {,}}
; T -> Terminal symbols 
; N -> NonTerminal symbols {}
; S -> Start Point
; P -> Production Rules :
                  ; exp -> number
                  ; exp -> symbol
                  ; exp -> + exp exp
                  ; exp -> - exp exp
                  ; exp -> * exp exp
                  ; exp -> ** exp exp
                  ; exp -> -1 * exp (The best one without problem) (Uniary Minus)
                  ; exp -> (exp)

       ;; Function definition
       ; F is a function
       ; Ls is a list of parameters
       ; B is body
       ; F -> (Name)Ls{B}
       ; Name -> symbol
       ; B-> exp
       ; Ls-> listOfSymbols

       ;; Function Application
       ;Fa is function application
       ;Fs is a function symbol
       ;La is a list of arguments
       ;Fa -> FsLa
       ;La  -> listOfSymbols
       ;Fs -> symbol
     


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
;; Function Application with multiple parameters.
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]

  [multC (l : ExprC) (r : ExprC)]

  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)]
  [factaccC (x : number) (acc : number)]
  )


;;lookup function takes n as a symbol and environment which includes binding values,
;; then it checks wheter this funciton in environment or not?
;;if there is,it produces value otherwise it gives error

(define (lookup [n : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "Symbol not found in env")]
    [(cons? env) (cond
                   [(equal? n (bind-name (first env))) (bind-val (first env))]
                   [else (lookup n (rest env))])]))
;Binding
;this function takes symbol as name and value which is number
;to bind any funciton

(define-type Binding
  [bind (name : symbol) (val : number)])

;; An alias to work easily on Environment.
(define-type-alias Env (listof Binding))

;; Empty environment.
(define mt-env empty)

;; Extending environment
(define extend-env cons)

(define-type Value
  [numvalue (n : number)]
  [functionvalue (params : (listof symbol)) (body : ExtendedMPF) (env : Env)])
;Extended data definition ,,Function data definition
;; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])

;; parse : s-exp -> ExprC
;; Purpose : To parse given s-exp to ExprC form

(define (parse [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'igz (s-exp->symbol (first sl)))
              (igz (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
           
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))
;; Tests :
"tests"
(test (parse (number->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))
(test (parse '(+ 3 4))(plusC (numC 3)(numC 4)))
(test (parse '(* 3 4))(multC (numC 3)(numC 4)))
(test (parse '(+ x x))(plusC (idC 'x)(idC 'x)))
(test (parse '(* x x))(multC (idC 'x)(idC 'x)))
(test (parse '(f (* x x)))(appC 'f (multC (idC 'x)(idC 'x))))
(test (parse '(igz 4 5 6))(igz (numC 4)(numC 5)(numC 6)))


;Contract
;;symbol (list of func definitions)-> : FunDefC
;Purpose
;; it takes a symobol and generate a function definition.

(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))
;(fdC 'triple  'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x)))

   (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))
 

 ;Contract
 ;; ExprC symbol ExprC -> ExprC
 ;Purpose
 ;; it takes a expression ( numC 7) , argument ('x) and the function it self. It produces the function with changes(numC 7) placed for every 'x in function
 ;;Examples
 ;;(subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) -> (plusC (plusC (numC 7) (numC 7)) (numC 7))
    (define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
 
     
                     
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]

     [factC (x) (factC (subst what for x))]
        [factaccC (x fact) (factaccC (subst what for x) (subst what for fact))]
     
     ;;[fibaccC (x fibC) (fibaccC (subst what for x) (subst what for fibC)(subst what for fibC));
     [igz (exp1 exp2 exp3) (igz (subst what for exp1) (subst what for exp2) (subst what for exp3))]))
 ;Tests for substitution
 (test (subst(numC 7) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 7) (numC 7)) (numC 7)))
 (test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
 ;(test (subst(numC 7 ,numC 8) '(x , y) (plusC (plusC (idC  'x) (idC  'y)) (idC 'x))) (plusC (plusC (numC 7) (numC 8)) (numC 7)))
 
 ;Contract
 ;;ExprC -> fds (listof FunDefC) - > number 
 ;Purpose
 ;;it takes an expression and list of function definitions and output a number (Function Application)
 ;;Examples
 ;(numC 7) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 7
 ;(igz(numC -5) (numC 1) (numC 0)) (fdC 'double  'x (plusC (idC  'x) (idC  'x))) -> 0
(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds))]
     [igz (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]
     [factaccC (x acc) (cond
                       [ (= x 1) acc]
                       [else ( interp (factaccC (- x 1) (* x acc)) fds )])]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))
  
 
 ;Tests for interp
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

(test (interp (multC (numC 10 ) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      60)
 ;Tests 
(test(interp(factC 4 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 24)
(test(interp(factC 5 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 120)
(test(interp(factC 3 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 6)
(test(interp(factC 2 )(fdC 'double  'x (plusC (idC  'x) (idC  'x)))) 2)
;(test (interp(factaccC 3 1 ) (plusC (multC (idC 'x) ))))

(test (interp (numC 4) (list)) 4)
(test (interp (plusC (numC 1) (numC 2)) (list)) 3)
(test (interp (multC (numC 5) (numC 2)) (list)) 10)
(test (interp (appC 'double (numC 10)) (list (fdC 'double 'x (plusC (idC 'x) (idC 'x))))) 20)


;; λ-expression grammar
;; λ-calc -> v
;; λ-calc -> (λ-calc λ-calc)
;; λ-calc -> (λ v λ-calc)
;; where v is a symbol.



;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; - λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

;; parse : s-exp -> λ-exp
;; Purpose : To transform given s-expression to corresponding
(define (parsel (sexp : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (cond
         [(= 2 (length sexp-list))
          (λ-app (parsel (first sexp-list))(parsel (second sexp-list)))]
         [(= 3 (length sexp-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                   (s-exp-symbol? (second sexp-list)))
              (λ-def (s-exp->symbol(second sexp-list))
                     (parsel (third sexp-list)))
              (error parsel "Not valid λ-definition")
              )]
         [else (error parsel "Not valid length λ-exp")]
         ))]
    [else (error parsel "Not valid λ-exp")]
))

;; Tests:
(test (parsel (symbol->s-exp 'y))(λ-sym 'y))
(test (parsel '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parsel '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (parsel '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (parsel '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))


;; unparse : λ-exp -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparse (le : λ-exp)) : s-expression
  (type-case λ-exp le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparse l)(unparse r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparse p))))
    ))

;; Test:
(test (unparse (λ-sym 'y))(symbol->s-exp 'y))
(test (unparse (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
               '((λ x x) y))
(test (unparse (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
       '((λ x x)(λ y y)))
      
(test (unparse (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))
       '(λ x (λ y (y x))))



;; A set represented as a list.
;; union : (listof symbol) (listof symbol) -> (listof symbol)
;; finding the union of two sets.
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; free-identifier : λ-calc -> (listof symbol)
;; Purpose : To find free identifiers in given λ expression.
(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))
(test (free-identifier (parsel '(λ x x))) empty)
(test (free-identifier (parsel '(λ x y))) (list 'y))
(test (free-identifier (parsel '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parsel '((λ f y)(λ z z)))) (list 'y))
