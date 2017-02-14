#lang plai-typed

; core language
;-------------------------

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define interp : (ArithC -> number)
  (lambda (a)
    (type-case ArithC a
      (numC (n) n)
      (plusC (l r) (+ (interp l) (interp r)))
      (multC (l r) (* (interp l) (interp r))))))


; surface language & desugar
;-------------------------
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define parse : (s-expression -> ArithS)
  (lambda (s)
    (cond ((s-exp-number? s) (numS (s-exp->number s)))
          ((s-exp-list? s) (let ((sl (s-exp->list s)))
                             (case (s-exp->symbol (first sl))
                               ('+ (plusS (parse (second sl)) (parse (third sl))))
                               ('* (multS (parse (second sl)) (parse (third sl))))
                               ('- (case (length sl)
                                     ((2) (uminusS (parse (second sl))))
                                     ((3) (bminusS (parse (second sl)) (parse (third sl))))
                                     (else (error 'parse "invalid3!"))))
                               (else (error 'parse "invalid input1!")))))
          (else (error 'parse "invalid input2!")))))


(define desugar : (ArithS -> ArithC)
  (lambda (as)
    (type-case ArithS as
      (numS (n) (numC n))
      (plusS (l r) (plusC (desugar l)
                          (desugar r)))
      (multS (l r) (multC (desugar l)
                          (desugar r)))
      (bminusS (l r) (plusC (desugar l)
                            (multC (numC -1)
                                   (desugar r))))
      (uminusS (e) (desugar (bminusS (numS 0) e))))))


;;test
(test (interp (desugar (parse '(* (* 2 3) (+ 2 3))))) 30)
(test (interp (desugar (parse '(- (* 2 3) (-(* 2 3)))))) 12)