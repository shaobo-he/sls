#lang racket

(require racket/set)
(require racket/file)

(provide (all-defined-out))

(define test-script
  "
   (declare-const x\n
   (_ BitVec 8))\n
   (declare-const y (_ BitVec 8))\n
   (assert (or (bvult (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))\n
               (= (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))))\n
   (assert (or (bvult y (_ bv2 8))\n
               (= y (_ bv2 8))))\n
   (assert (not (bvult (bvadd x y) (_ bv1 8))))")

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

(define file->sexp
  (λ (fn)
    (string->sexp (file->string fn))))

(define transform-expr
  (λ (sexp)
    (match sexp
      [`(not, exprs ...) `(¬ ,@(map transform-expr exprs))]
      [`(and, exprs ...) `(∧ ,@(map transform-expr exprs))]
      [`(or, exprs ...) `(∨ ,@(map transform-expr exprs))]
      [`(implies, expr1, expr2) (transform-expr `(or (not, expr1) ,expr2))]
      [`(bvule, exprs ...) (let ([tes (map transform-expr exprs)]) `(∨ (bvult ,@tes) (= ,@tes)))]
      [`(,op, exprs ...) `(,op ,@(map transform-expr exprs))]      
      [else sexp])))

(define get-formula
  (λ (cmds)
    (let ([get-expr
              (λ (cmd result)
                (match cmd
                  [`(assert, expr) (list '∧ (transform-expr expr) result)]
                  [else result]))])
      (foldl get-expr '⊤ cmds))))

(define get-vars
  (λ (cmds)
    (let ([get-var
           (λ (cmd result)
             (match cmd
               [`(declare-const ,id (_ BitVec ,size)) (cons (cons id size) result)]
               [else result]))])
      (foldl get-var '() cmds))))

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

(define get/assertions
  (λ (F)
    (match F
      [`(∧ ,as ...) (filter list? as)]
      [_ (error "not a valid SMT formula")])))

(define get/vars
  (λ (F assignment)
    (letrec ([get/vars/do (λ (F)
                            (match F
                              [`(,op ,fs ...) (apply append (map (λ (f) (get/vars/do f)) fs))]
                              [_ (if (number? F)
                                     '()
                                     (if (hash-has-key? assignment (symbol->string F))
                                         `(,F)
                                         '()))]))])
      (set->list (list->set (get/vars/do F))))))

(define test-formula (unnest (formula->nnf (get-formula (string->sexp test-script)))))
(define test-vars (get-vars (string->sexp test-script)))
(define kenken-formula (unnest (formula->nnf (get-formula (file->sexp "test/test3.smt2")))))
(define kenken-vars (get-vars (file->sexp "test/test3.smt2")))