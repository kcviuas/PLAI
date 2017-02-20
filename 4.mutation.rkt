#lang plai-typed

;;core---------------------------
(define-type ExprC
  [numC  (n : number)]
  [plusC (l : number) (r : number)]
  [multC (l : number) (r : number)]
  [idC   (s : symbol)]
  [appC  (fun : ExpC) (arg : ExpC)]
  [lamC  (arg : symbol) (body : ExpC)]
  [boxC  (arg : ExpC)]
  [unboxC (arg : ExpC)]
  [setboxC (b : ExpC) (v : ExpC)])


;value
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV  (loc : Location)])



;environment
(define-type-alias Location number)

(define-type Env
  [empty-env]
  [extend-env (name : symbol) (loc : Location) (env : Env)])


(define lookup : (symbol Env -> Location)
  (lambda (name env)
    (type-case Env env
      [empty-env () (error 'lookup "error! unbounded name in environment.")]
      [extend-env (n loc env0) (if (equal? n name)
                                   loc
                                   (lookup name env0))])))



;store
(define-type-alias Store (listof Value))

(define fetch : (Location Store -> Value)
  (lambda (loc sto)
    (let ([len (length sto)])
      (if (equal? len 0)
          (error 'fetch "error! this should not happen!")
          (if (equal? len loc)
              (first sto)
              (fetch loc (rest sto)))))))

(define override-store : (Store Location Value -> Store)
  (lambda (s loc v)
    (if (= (length s) loc)
        (cons v (rest s))
        (cons (first s) (override-store (rest s) loc v)))))

;eval
(define-type Result
  [v&s (val : Value) (sto : Store)])


(define interp : (ExprC Env Store -> Result)
  (lambda (exp env sto)
    (type-case ExprC exp
      [numC (n) (v&s (numV n) sto)]
      [plusC (l r) (let ([res1 (interp l env sto)])
                     (let ([res2 (interp r env (v&s-sto res1))])
                       (v&s (numV (+ (numV-n (v&s-val res1))
                                     (numV-n (v&s-val res2))))
                            (v&s-sto res2))))]
      [multC (l r) (type-case (interp l env sto)
                     [v&s (val1 sto1)
                          (type-case (interp e env sto1)
                            [v&s (val2 sto2)
                                 (v&s (numV (+ (numV-n val1)
                                               (numV-n val2)))
                                      sto2)])])]
      [idC (s) (let ([val (fetch (lookup s env) sto)])
                 (v&s val sto))]
      [lamC (s body) (v&s (closV s body env) sto)]
      [appC (f arg) (type-case (interp f env sto)
                      [v&s (val1 sto1)
                           (type-case (interp arg env sto1)
                             [v&s (val2 sto2)
                                  (interp (closV-body val1)
                                          (extend-env (closV-arg val1) (+ (length sto2) 1) (closV-env val1))
                                          (cons val2 sto2))])])]
      [boxC (e) ]
      [unboxC (e) ]
      [setboxC (b v) ])))




;;surface---------------------------

(define-type ExprS
  []
  []
  [])


(define desugar : (ExprS -> ExprC)
  (lambda ()
    ()))


;;test---------------------------
