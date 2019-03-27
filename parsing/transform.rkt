#lang racket

(require "../data/bit-vec.rkt")
(require "../data/fp.rkt")

(provide (all-defined-out))

(define transform-expr
  (λ (sexp)
    (match sexp
      [`(not, exprs ...) `(¬ ,@(map transform-expr exprs))]
      [`(and, exprs ...) `(∧ ,@(map transform-expr exprs))]
      [`(or, exprs ...) `(∨ ,@(map transform-expr exprs))]
      [`(implies, expr1, expr2) (transform-expr `(or (not, expr1) ,expr2))]
      [`(=>, expr1, expr2) (transform-expr `(or (not, expr1) ,expr2))]
      ; not sure why this is commented
      ; maybe it's related performance as we compute the same expression twice
      ;[`(bvule, exprs ...) (let ([tes (map transform-expr exprs)]) `(∨ (bvult ,@tes) (= ,@tes)))]
      [`(fp.leq, exprs ...) (transform-expr `(∨ (fp.eq ,@exprs) (fp.lt ,@exprs)))]
      ;[`(,op, exprs ...) `(,op ,@(map transform-expr exprs))]
      [`(,exprs ...) (map transform-expr exprs)]
      ['true '⊤]
      ['false '⊥]
      [_ sexp])))

; shamelessly stole it from https://homes.cs.washington.edu/~emina/media/sat/code.html#normal-formsrkt
(define formula->nnf
  (λ (formula)
    (match formula
      [`(¬ ⊥) `⊤]
      [`(¬ ⊤) `⊥]
      [`(¬ (¬, f)) (formula->nnf f)]
      [`(¬ (∧ ,fs ...))
       `(∨ ,@(for/list ([fi fs]) (formula->nnf `(¬ ,fi))))]
      [`(¬ (∨ ,fs ...))
       `(∧ ,@(for/list ([fi fs]) (formula->nnf `(¬ ,fi))))]
      [`(,op ,fs ...)
       `(,op ,@(map formula->nnf fs))]
      [_ formula])))

(define (unnest f)
  (match f
    [`(∨ ,gs ... (∨ ,fs ...) ,hs ...)
     (unnest `(∨ ,@gs ,@fs ,@hs))]
    [`(∧ ,gs ... (∧ ,fs ...) ,hs ...)
     (unnest `(∧ ,@gs ,@fs ,@hs))]
    [`(,op ,fs ...)
     `(,op ,@(map unnest fs))]
    [_ f]))

;; logical simplifications
;; TODO: use z3 simplications (might be pita to implement via ffi)
(define simplify
  (λ (sexp)
    (define simplify∧∨
      (λ (es cond-sym term-sym)
        (define simplify∧∨-helper
          (λ (es col)
            (cond
              [(empty? es) (reverse col)]
              [else (let ([e (car es)])
                      (cond
                        [(eq? e cond-sym) (simplify∧∨-helper (cdr es) col)]
                        [(eq? e term-sym) term-sym]
                        [else (simplify∧∨-helper (cdr es) (cons e col))]))])))
        (let ([result (simplify∧∨-helper es '())])
          (match result
            [`(∧ ,v) v]
            [`(∨ ,v) v]
            [else result]))))
    (match sexp
      [`(¬ (¬ ,e)) (simplify e)]
      ['(¬ ⊤) '⊥]
      ['(¬ ⊥) '⊤]
      [`(∧ ,gs ...)
       (simplify∧∨ `(∧ ,@(map simplify gs)) '⊤ '⊥)]
      [`(∨ ,gs ...)
       (simplify∧∨ `(∨ ,@(map simplify gs)) '⊥ '⊤)]
      [`(,gs ...) (map simplify gs)]
      [else sexp])))

;; fp simplifications
(define extract-bv-value
  (λ (bv)
    (string->number
     (substring
      (symbol->string bv)
      2))))

(define remove-const-bv2fp
  (λ (sexp)
    (match sexp
      [`((_ to_fp ,exp ,sig) (_ ,bv ,width))
       (BitVec->FloatingPoint
        (mkBV
         width
         (extract-bv-value bv))
        exp
        sig)]
      [`(,exps ...) `(,@(map remove-const-bv2fp exps))]
      [_ sexp])))

;; e.g., (fp (_ bv0 1) (_ bv133 8) (_ bv7340032 23))
(define remove-fpconst
  (λ (sexp)
    (match sexp
      [`(fp (_ ,sign ,sig-width) (_ ,exp ,exp-width) (_ ,sig, sig-width-wo))
       (BitVec->FloatingPoint
        (mkBV
         (+ (+ sig-width-wo 1) exp-width)
         (+
          (arithmetic-shift
           (extract-bv-value sign)
           (+ exp-width sig-width-wo))
          (+
           (arithmetic-shift
            (extract-bv-value exp)
            sig-width-wo)
           (extract-bv-value sig))))
        exp-width
        (+ sig-width-wo 1))]
      [`(_ +zero ,exp-width ,sig-width)
       (real->FloatingPoint 0.0 exp-width sig-width)]
      [`(_ -zero ,exp-width ,sig-width)
       (real->FloatingPoint -0.0 exp-width sig-width)]
      [`(_ +oo ,exp-width ,sig-width)
       (real->FloatingPoint +inf.0 exp-width sig-width)]
      [`(_ -oo ,exp-width ,sig-width)
       (real->FloatingPoint -inf.0 exp-width sig-width)]
      [`(_ NaN ,exp-width ,sig-width)
       (real->FloatingPoint +nan.0 exp-width sig-width)]
      [`(,exps ...) `(,@(map remove-fpconst exps))]
      [_ sexp])))

; equality simplifications
; assume there's no duplicate equality assertion
(define remove-equalities
  (λ (asserts var-info)
    (define collect-equalities
      (λ (assert eqs)
        (define try-insert-eqs
          (λ (eqs vs cols hit?)
            (cond
              [(empty? eqs) (if hit? cols (cons (list->set vs) cols))] ; if hit ignore else add group to list
              [else (let ([eq (car eqs)])
                      (if
                       hit?
                       (try-insert-eqs (cdr eqs) vs (cons eq cols) hit?) ; already hit, continue
                       (if
                        (ormap
                         (λ (v) (set-member? eq v))
                         (filter symbol? vs)) ; found the group
                        (if (and (not (andmap symbol? (set->list eq)))
                                 (not (andmap symbol? vs)))
                            ; this means we have existing equalties like a = (+ b c)
                            ; and we try to insert a = (+ d f)
                            ; so we need to abort
                            (try-insert-eqs (cdr eqs) vs cols #t)
                            (try-insert-eqs
                             (cdr eqs)
                             vs
                             (cons (set-union (list->set vs) eq) cols)
                             #t))
                        (try-insert-eqs (cdr eqs) vs (cons eq cols) hit?))))])))
        (match assert
          [`(= ,lhs ,rhs)
           (cond
             [(and (symbol? lhs) (symbol? rhs)) (try-insert-eqs eqs `(,lhs ,rhs) '() #f)]
             [(symbol? lhs) (try-insert-eqs eqs `(,lhs ,rhs) '() #f)]
             [(symbol? rhs) (try-insert-eqs eqs `(,lhs ,rhs) '() #f)]
             [else eqs])]
          [_ eqs])))
    (define is-eq?
      (λ (v eqs)
        (ormap (λ (eq) (set-member? eq v)) eqs)))
    (define remove-eq-asserts
      (λ (asserts eqs cols)
        (cond
          [(empty? asserts) cols]
          [else (let ([assert (car asserts)])
                  (match assert
                    [`(= ,lhs ,rhs)
                     (cond
                       [(symbol? lhs)
                        (if (is-eq? lhs eqs)
                            (remove-eq-asserts (cdr asserts) eqs cols)
                            (remove-eq-asserts (cdr asserts) eqs (cons assert cols)))]
                       [(symbol? rhs)
                        (if (is-eq? rhs eqs)
                            (remove-eq-asserts (cdr asserts) eqs cols)
                            (remove-eq-asserts (cdr asserts) eqs (cons assert cols)))]
                       [else (remove-eq-asserts (cdr asserts) eqs (cons assert cols))])]
                    [_ (remove-eq-asserts (cdr asserts) eqs (cons assert cols))]))])))
        (let ([eqs (foldl collect-equalities '() asserts)])
          ;(replace (remove-eq-asserts))
          (if (empty? eqs)
              asserts
              (remove-eq-asserts asserts eqs '())))))

(define remove-let-bindings
  (λ (sexp)
    (define do
      (λ (sexp env)
        (match sexp
          [`(let (,bindings ...) ,body)
           (let ([new-env
                  (foldl
                   (λ (binding env)
                     (extend-env (car binding) (do (car (cdr binding)) env) env))
                   env
                   bindings)])
             (do body new-env))]
          [`(, exprs ...) (map (λ (exp) (do exp env)) exprs)]
          [_ (if (symbol? sexp) (lookup sexp env) sexp)])))
    (define lookup
      (λ (sym env)
        (cond
          [(null? env) sym]
          [else (let ([binding (car env)])
                  (if (equal? (car binding) sym)
                      (cdr binding)
                      (lookup sym (cdr env))))])))
    (define extend-env (λ (sym val env) (cons (cons sym val) env)))
    (do sexp '())))