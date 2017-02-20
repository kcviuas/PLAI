#lang plai-typed

;;core---------------------------
(define-type ExprC
  [numC  (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC   (n : symbol)]
  [lamC  (arg : symbol) (body : ExprC)]
  [appC  (fun : ExprC) (param : ExprC)])



;environment
(define-type Env
  [empty-env]
  [extend-env (name : symbol) (val : Value) (env : Env)])

(define lookup : (symbol Env -> Value)
  (lambda (s env)
    (type-case Env env
      [empty-env () (error 'lookup "unbound identifer in environment!")]
      [extend-env (name val env0) (if (equal? s name)
                                      val
                                      (lookup s env0))])))


;value
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])



;eval
(define interp : (ExprC Env -> Value)
  (lambda (exp env)
    (type-case ExprC exp
      [numC  (n) (numV n)]
      [plusC (l r) (numV (+ (numV-n (interp l env))
                            (numV-n (interp r env))))]
      [multC (l r) (numV (* (numV-n (interp l env))
                            (numV-n (interp r env))))]
      [idC   (s) (lookup s env)]
      [lamC  (arg body) (closV arg body env)]
      [appC  (fun param) (let ((clos (interp fun env))
                               (val  (interp param env)))
                           (interp (closV-body clos)
                                   (extend-env (closV-arg clos)
                                               val
                                               env)))])))




;surface---------------------------
(define-type ExprS
  [numS  (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS   (n : symbol)]
  [lamS  (arg : symbol) (body : ExprS)]
  [appS  (fun : ExprS) (param : ExprS)]
  [letS  (id : symbol) (exp : ExprS) (body : ExprS)])


(define desugar : (ExprS -> ExprC)
  (lambda (expS)
    (type-case ExprS expS
      [numS  (n) (numC n)]
      [plusS (l r) (plusC (desugar l) (desugar r))]
      [multS (l r) (multC (desugar l) (desugar r))]
      [idS   (n) (idC n)]
      [lamS  (arg body) (lamC arg (desugar body))]
      [appS  (fun param) (appC (desugar fun) (desugar param))]
      [letS  (id exp body) (appC (lamC id (desugar body))
                                 (desugar exp))])))





;;test---------------------------
(define prog (letS 'f (lamS 'x (plusS (idS 'x) (numS 1))) (appS (idS 'f) (numS 5))))
(test (interp (desugar prog) (empty-env)) (numV 6))