#lang racket

(provide (all-defined-out))

(define test-str "; Modeling sequential code with bitvectors
;; Correct swap with no temp var
; int x, y;
; x = x + y;
; y = x - y;
; x = x - y;

(set-logic QF_BV)
(set-option :produce-models true)

(declare-const x_0 (_ BitVec 32))
(declare-const x_1 (_ BitVec 32))
(declare-const x_2 (_ BitVec 32))
(declare-const y_0 (_ BitVec 32))
(declare-const y_1 (_ BitVec 32))
(assert (= x_1 (bvadd x_0 y_0)))
(assert (= y_1 (bvsub x_1 y_0)))
(assert (= x_2 (bvsub x_1 y_1)))

(assert (not
  (and (= x_2 y_0)
       (= y_1 x_0))))
(check-sat)
; unsat
(exit)
")

(define string->sexp
  (λ (str)
    (let ([fd (open-input-string str)])
      (letrec
          ([parse-string^
            (λ (fd)
              (let ([c (read fd)])
                (cond
                  [(eq? c eof) '()]
                  [else (cons c (parse-string^ fd))])))])
        (parse-string^ fd)))))

(define transform-expr
  (λ (sexp)
    (match sexp
      [`(not, exprs ...) `(¬ ,@(map transform-expr exprs))]
      [`(and, exprs ...) `(∧ ,@(map transform-expr exprs))]
      [`(or, exprs ...) `(∨ ,@(map transform-expr exprs))]
      [`(implies, expr1, expr2) (transform-expr `(or (not, expr1) ,expr2))]
      [`(,op, exprs ...) `(,op ,@(map transform-expr exprs))]      
      [else sexp])))

(define get-formula
  (λ (cmds)
    (letrec ([get-expr
              (λ (cmd result)
                (match cmd
                  [`(assert, expr) (list '∧ (transform-expr expr) result)]
                  [else result]))])
      (foldl get-expr '⊤ cmds))))

(define atom?
  (λ (sexp)
    (not (list? sexp))))

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
