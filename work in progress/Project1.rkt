;; ExprC data definition
;; 
;; -------------------

#lang plai-typed

;; Data definition for ExprC
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [expC (l : ExprC) (r : ExprC)]
  [factC (n : number)]
  [ifgz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factaccC (n : number) (acc : number)])

;; Multiple parameter function definition
(define-type FDC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

;; 
