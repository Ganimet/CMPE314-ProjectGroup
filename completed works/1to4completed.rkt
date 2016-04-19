;; Furkan Karakoyunlu
;; 112200036

#lang racket
(require plai-typed)

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)])


;; Function def with multiple parameters
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

;; Parser
;; Parse fiven s-exp to ExprC
;; s-exp -> ExprC
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
            [(-) (subC (parse (second sl)) (parse (third sl)))]
            [(**) (expC (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))

;; Tests
(test (parse (number->s-exp 2)) (numC 2))
(test (parse (symbol->s-exp 'a)) (idC 'a))
(test (parse '(+ 5 2)) (plusC (numC 5) (numC 2)))
(test (parse '(- 1 3)) (subC (numC 1) (numC 3)))
(test (parse '(* 7 8)) (multC (numC 7) (numC 8)))
(test (parse '(** 7 3)) (expC (numC 7) (numC 3)))
(test (parse '(+ (- 1 2) (* 3 4))) (plusC (subC (numC 1) (numC 2)) (multC (numC 3) (numC 4))))
(test (parse '(+ a b)) (plusC (idC 'a) (idC 'b)))
(test (parse '(f (** x y))) (appC 'f (expC (idC 'x) (idC 'y))))
(test (parse '(igz 1 2 3)) (igz (numC 1) (numC 2) (numC 3)))


;; symbol -> FunDefC
(fdC 'double  '(x , y) (plusC (idC  'x) (idC  'y)))


;; FunDefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds) (error 'get-fundef "reference to undefined function")]
     [(cons? fds) (cond
                    [(equal? n (fdC-name (first fds))) (first fds)]
                    [else (get-fundef n (rest fds))])]))

;; ExprC symbol ExprC -> ExprC
;; it takes a expression (numC 2), argument ('x) and the function itself,
;; and it produces the function with changes(numC 2) placed for every 'x in function
(define (subst [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
     (type-case ExprC in
     [numC (n) in]
     [idC (s) (cond
              [(symbol=? s for) what]
              [else in])]
     [appC (f a) (appC f (subst what for a))]
     [plusC (l r) (plusC (subst what for l)
                         (subst what for r))]
 
     [subC (l r) (plusC (subst what for l)
                         (subst what for r))]
     [multC (l r) (multC (subst what for l)
                         (subst what for r))]
     [expC (l r) (multC (subst what for l)
                         (subst what for r))]
     [factC (x) (factC (subst what for x))]
     [igz (exp1 exp2 exp3) (igz (subst what for exp1) (subst what for exp2) (subst what for exp3))]))

;; Tests
(test (subst (numC 5) 'x (plusC (plusC (idC  'x) (idC  'x)) (idC 'x))) (plusC (plusC (numC 5) (numC 5)) (numC 5)))
(test (subst(plusC (numC 3) (numC 4)) 'y (plusC (multC (idC  'y) (idC  'y)) (idC 'y))) (plusC (multC (plusC (numC 3) (numC 4)) (plusC (numC 3) (numC 4))) (plusC (numC 3) (numC 4))))
 

;; Interp
;; ExprC -> fds -> number
;; it takes an expression and list of function definitions and output 
;; a number
;; Function Application
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
               (interp (subst a
                              (fdC-arg fd)
                              (fdC-body fd))
                       fds))]
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp exp1 fds) 0) (interp exp2 fds)]
                        [else (interp exp3 fds)])]
   [plusC (l r) (+ (interp l fds) (interp r fds))]
   [subC (l r) (- (interp l fds) (interp r fds))]
   [multC (l r) (* (interp l fds) (interp r fds))]
   [expC (l r) (expt (interp l fds) (interp r fds))]
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp (factC (- x 1)) fds))])]))


;; TESTS
(test (interp(numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp(numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp(numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp(numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp(numC 55) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for igz (if greater than zero)
(test (interp(igz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp(igz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp(igz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp(igz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp(igz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 
;; Tests for plus operation
(test (interp(plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp(plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp(plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp(plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp(plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp(subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp(subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp(subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp(subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp(subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
;; Tests for multiplaction
(test (interp(multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp(multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp(multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp(multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp(multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
;; Tests for exponention operation
(test (interp(expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp(expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp(expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp(expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp(expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)

;; Tests for factorial operation
(test (interp(factC 1)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp(factC 2)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp(factC 3)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp(factC 4)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp(factC 5)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 



;; interp2 LAZY APPROACH
(define (interp2 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error 'interp2 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])
                 
               (interp2 (subst a (fdC-arg fd) (fdC-body fd)) fds))]
            
                
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp2 exp1 fds) 0) (interp2 exp2 fds)]
                        [else (interp2 exp3 fds)])]
   [plusC (l r) (+ (interp2 l fds) (interp2 r fds))]
   [subC (l r) (- (interp2 l fds) (interp2 r fds))]
   [multC (l r) (* (interp2 l fds) (interp2 r fds))]
   [expC (l r) (* (interp2 l fds) (interp2 r fds))]
   ;; expC is taking * because I didnt write the ** operation
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp2 (factC (- x 1)) fds))])]))


;; interp3 EAGER APPROACH
(define (interp3 [e : ExprC] [fds : (listof FunDefC)]) : number
   (type-case ExprC e
   [numC (n) n]
   [idC (_) (error interp3 "shouldn't get here")]
   [appC (f a) (local ([define fd (get-fundef f fds)])


                
                 (interp3 (subst (numC (interp3 a fds))
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
   [igz (exp1 exp2 exp3) (cond
                           [(> (interp3 exp1 fds) 0) (interp3 exp2 fds)]
                        [else (interp3 exp3 fds)])]
   [plusC (l r) (+ (interp3 l fds) (interp3 r fds))]
   [subC (l r) (- (interp3 l fds) (interp3 r fds))]
   [multC (l r) (* (interp3 l fds) (interp3 r fds))]
   [expC (l r) (* (interp3 l fds) (interp3 r fds))]
   ;; expC is taking * because I didnt write the ** operation
   [factC (x) (cond
               [(= x 1) 1]
               [else (* x (interp3 (factC (- x 1)) fds))])]))


;; TESTS FOR INTERP 2 (LAZY APPROACH)
(test (interp2 (numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp2 (numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp2 (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp2 (numC 55) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for igz (if greater than zero)
(test (interp2 (igz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp2 (igz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp2 (igz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 
;; Tests for plus operation
(test (interp2 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp2 (plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp2 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp2 (plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp2 (plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp2 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp2 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp2 (subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
;; Tests for multiplaction
(test (interp2 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp2 (multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp2 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp2 (multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp2 (multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
;; Tests for exponention operation
(test (interp2 (expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp2 (expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp2 (expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp2 (expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp2 (expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)

;; Tests for factorial operation
(test (interp2 (factC 1)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp2 (factC 2)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp2 (factC 3)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp2 (factC 4)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp2 (factC 5)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 



;; TESTS FOR INTERP 3 (EAGER APPROACH)
(test (interp3 (numC 5) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 5)
(test (interp3 (numC 15) (fdC 'double  'x (plusC (idC 'x) (idC 'x)))) 15)
(test (interp3 (numC 2) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (numC 7) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 7)
(test (interp3 (numC 55) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 55)

;; Tests for igz (if greater than zero)
(test (interp3 (igz(numC 5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC -5) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
(test (interp3 (igz(numC 55) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC 555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 1)
(test (interp3 (igz(numC -5555) (numC 1) (numC 0)) (fdC 'double 'x (plusC (idC 'x) (idC  'x)))) 0)
 
;; Tests for plus operation
(test (interp3 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 12)
(test (interp3 (plusC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 19)
(test (interp3 (plusC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 12)
(test (interp3 (plusC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 229)
(test (interp3 (plusC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC  'x) (idC 'x)))) 74)
 
;; Tests for subtraction operation
(test (interp3 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (subC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 3)
(test (interp3 (subC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (subC (numC 100) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -29)
(test (interp3 (subC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) -6)
 
;; Tests for multiplaction
(test (interp3 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp3 (multC (numC 11) (numC 8)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 88)
(test (interp3 (multC (numC 7) (numC 5)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 35)
(test (interp3 (multC (numC 10) (numC 129)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1290)
(test (interp3 (multC (numC 34) (numC 40)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1360)
 
;; Tests for exponention operation
(test (interp3 (expC (numC 2) (numC 4)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 16)
(test (interp3 (expC (numC 11) (numC 2)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 121)
(test (interp3 (expC (numC 7) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 343)
(test (interp3 (expC (numC 10) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1000)
(test (interp3 (expC (numC 4) (numC 3)) (fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 64)

;; Tests for factorial operation
(test (interp3 (factC 1)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 1)
(test (interp3 (factC 2)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 2)
(test (interp3 (factC 3)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 6)
(test (interp3 (factC 4)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 24)
(test (interp3 (factC 5)(fdC 'double 'x (plusC (idC 'x) (idC 'x)))) 120) 




;; λ-expression grammar
;; LAMBDA -> v
;; LAMBDA -> (LAMBDA LAMBDA)
;; LAMBDA -> (λ v LAMBDA)


;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp)))

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))
(λ-def 'v  (λ-sym 'x))
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