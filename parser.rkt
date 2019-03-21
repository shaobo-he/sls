#lang racket

(require racket/set)
(require racket/file)
(require "data/bit-vec.rkt")
(require "data/fp.rkt")
(require math/bigfloat)

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
      ;[`(bvule, exprs ...) (let ([tes (map transform-expr exprs)]) `(∨ (bvult ,@tes) (= ,@tes)))]
      [`(,op, exprs ...) `(,op ,@(map transform-expr exprs))]      
      [else sexp])))

(define remove-const-bv2fp
  (λ (sexp)
    (match sexp
      [`((_ to_fp ,exp ,sig) (_ ,bv ,width)) (bigfloat->flonum (BitVec->FloatingPoint
                                              (mkBV width
                                                    (string->number
                                                     (substring
                                                      (symbol->string bv)
                                                      2))) exp sig))]
      [`(,exps ...) `(,@(map remove-const-bv2fp exps))]
      [_ sexp])))

(define remove-let-bindings
  (λ (sexp)
    (letrec ([do (λ (sexp env)
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
                     [_ (if (symbol? sexp) (lookup sexp env) sexp)])
                   )]
             [lookup (λ (sym env)
                       (cond
                         [(null? env) sym]
                         [else (let ([binding (car env)])
                                 (if (equal? (car binding) sym)
                                     (cdr binding)
                                     (lookup sym (cdr env))))]))]
             [extend-env (λ (sym val env) (cons (cons sym val) env))])
      (do sexp '()))))
#|
(define let-script "(assert
 (not (fp.isNaN ((_ to_fp 11 53) a_ackermann!0))))
(assert
 (let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
 (let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
 (not (fp.eq ?x14 ((_ to_fp 11 53) (_ bv0 64)))))))
(assert
 (let ((?x30 (fp.add roundNearestTiesToEven (fp.mul roundNearestTiesToEven ((_ to_fp 11 53) a_ackermann!0) ((_ to_fp 11 53) (_ bv4652104065864433664 64))) ((_ to_fp 11 53) (_ bv4638321995473517281 64)))))
(let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
(let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
(let ((?x22 (fp.add roundNearestTiesToEven ((_ to_fp 11 53) (_ bv0 64)) (fp.mul roundNearestTiesToEven ?x14 ((_ to_fp 11 53) (_ bv4652104065864433664 64))))))
(let ((?x28 (fp.add roundNearestTiesToEven (fp.add roundNearestTiesToEven ?x22 ((_ to_fp 11 53) (_ bv4638321995473517281 64))) ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))
(not (fp.eq ?x28 (fp.add roundNearestTiesToEven ?x30 ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))))))))
")
|#
(define let-script "(assert                                                                         
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))                                  
 (fp.geq ?x8 ((_ to_fp 11 53) (_ bv4487126258331716666 64)))))                  
(assert                                                                         
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))                                  
 (fp.leq ?x8 ((_ to_fp 11 53) (_ bv4487186704622697397 64)))))
(assert                                                                         
 (let ((?x52 ((_ to_fp 11 53) (_ bv4607182418800017408 64))))                   
 (let ((?x41 (bvadd (_ bv4293918720 32) (bvor (bvand (bvand ((_ extract 63 32) fresh_to_ieee_bv_!2) (_ bv2147483647 32)) (_ b
v1048575 32)) (_ bv1072693248 32)))))
 (let ((?x45 (concat ((_ extract 15 8) ?x41) (concat ((_ extract 7 0) ?x41) ((_ extract 31 0) fresh_to_ieee_bv_!1)))))
 (let ((?x50 ((_ to_fp 11 53) (concat ((_ extract 31 24) ?x41) (concat ((_ extract 23 16) ?x41) ?x45)))))
 (let ((?x55 (fp.div roundNearestTiesToEven ?x52 (fp.add roundNearestTiesToEven ?x50 ?x52))))
 (let ((?x53 (fp.sub roundNearestTiesToEven ?x50 ?x52)))                        
 (let ((?x56 (fp.mul roundNearestTiesToEven ?x53 ?x55)))                        
 (= ?x56 ((_ to_fp 11 53) fresh_to_ieee_bv_!3))))))))))
")
(define sexp (string->sexp let-script))
(remove-let-bindings sexp)

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