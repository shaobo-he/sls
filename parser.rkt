#lang racket

(provide (all-defined-out))

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
    (match sexp
      [`(= ,ops ...) #t]
      [`(bvult ,ops ...) #t]
      [`(,op ...) #f]
      [else #t])))

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
