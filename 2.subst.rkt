#lang plai-typed

; core language
;-------------------------

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type FunDefC
  [fdC (name : symbol)
       (arg : symbol)
       (body : ExprC)])

(define subst : (ExprC symbol ExprC -> ExprC)
  (lambda (what for in)
    (type-case ExprC in
      (numC (n) in)
      (idC (s) (cond ((equal? s for) what)
                     (else in)))
      (appC (fun arg) (appC fun (subst what for arg)))
      (plusC (l r) (plusC (subst what for l)
                          (subst what for r)))
      (multC (l r) (multC (subst what for l)
                          (subst what for r))))))


(define get-fundef : (symbol (listof FunDefC) -> FunDefC)
  (lambda (n fds)
    (cond ((empty? fds) (error 'get-fundef "reference to undefined function!"))
          (else (cond ((equal? n (fdC-name (first fds))) (first fds))
                      (else (get-fundef n (rest fds))))))))


(define interp : (ExprC (listof FunDefC) -> number)
  (lambda (e fds)
    (type-case ExprC e
      (numC (n) n)
      (plusC (l r) (+ (interp l fds) (interp r fds)))
      (multC (l r) (* (interp l fds) (interp r fds)))
      (appC (f a) (let ((fd (get-fundef f fds)))
                    (interp (subst a (fdC-arg fd) (fdC-body fd))
                            fds)))
      (idC (_) (error 'interp "interp shouldn't get here!")))))


; surface language & desugar
;-------------------------
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (s : symbol)]
  [appS (name : symbol)
        (arg : ExprS)])

(define parse : (s-expression -> ExprS)
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
                               (else (appS (s-exp->symbol (first sl)) (parse (second sl))))
                               ;(else (error 'parse "invalid input1!"))
                               )))
          (else (error 'parse "invalid input2!")))))


(define desugar : (ExprS -> ExprC)
  (lambda (as)
    (type-case ExprS as
      (numS (n) (numC n))
      (plusS (l r) (plusC (desugar l)
                          (desugar r)))
      (multS (l r) (multC (desugar l)
                          (desugar r)))
      (bminusS (l r) (plusC (desugar l)
                            (multC (numC -1)
                                   (desugar r))))
      (uminusS (e) (desugar (bminusS (numS 0) e)))
      (idS (s) (idC s))
      (appS (n a) (appC n (desugar a))))))

(define fds
  (list (fdC 'f 'x (plusC (idC 'x) (idC 'x)))
        (fdC 'g 'x (multC (idC 'x) (idC 'x)))))


;;test
(test (interp (desugar (parse '(* (f 2) (g 3)))) fds) 36)
(test (interp (desugar (parse '(- (f (g 2))))) fds) -8)