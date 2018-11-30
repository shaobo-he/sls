#lang racket

(require "data.rkt")

(provide (all-defined-out))

(define get-bv
  (λ (assignment sym)
    (hash-ref assignment (symbol->string sym))))


; the evaluator
(define eval
  (λ (be assignment)
    (letrec ([eval^ (λ (be)
                      (match be
                        [`(bvneg ,op) (eval/bvneg (eval^ op))]
                        [`(bvadd ,op1 ,op2) (eval/bvadd (eval^ op1) (eval^ op2))]
                        [`(bvsub ,op1 ,op2) (eval/bvsub (eval^ op1) (eval^ op2))]
                        [`(bvmul ,op1 ,op2) (eval/bvmul (eval^ op1) (eval^ op2))]
                        [`(bvudiv ,op1 ,op2) (eval/bvudiv (eval^ op1) (eval^ op2))]
                        [`(bvurem ,op1 ,op2) (eval/bvurem (eval^ op1) (eval^ op2))]
                        [`(bvnot ,op) (eval/bvnot (eval^ op))]
                        [`(bvand ,op1 ,op2) (eval/bvand (eval^ op1) (eval^ op2))]
                        [`(bvor ,op1 ,op2) (eval/bvor (eval^ op1) (eval^ op2))]
                        [`(_ ,op1, op2) (mkBV (string->number
                                               (substring
                                                (symbol->string op1)
                                                (string-length "bv"))) op2)]
                        [`(,op ...) (error "unsupported operations")]
                        [else (get-bv assignment be)]))])
      (eval^ be))))

(define Hamming-distance
  (λ (bv1 bv2)
    (let ([bs1 (BitVec->bits bv1)]
          [bs2 (BitVec->bits bv2)])
      (foldl (λ (t r) (+ r (bitwise-xor (car t) (cdr t))))
             0
             (map cons bs1 bs2)))))

(define score=
  (λ (c)
    (λ (bv1 bv2)
      (if (bv= bv1 bv2)
          1.0
          (* c (- 1.0
                  (/ (Hamming-distance bv1 bv2)
                     (BitVec-width bv1))))))))

(define score!=
  (λ (bv1 bv2)
    (if (bv= bv1 bv2)
        0.0
        1.0)))

(define score<
  (λ (c)
    (λ (bv1 bv2)
      (if (bv< bv1 bv2)
          1.0
          (* c (- 1.0 (/ (- (BitVec-value bv1) (BitVec-value bv2))
                         (expt 2 (BitVec-width bv1)))))))))

(define score!<
  (λ (c)
    (λ (bv1 bv2)
      (if (bv< bv1 bv2)
          (* c (- 1.0 (/ (- (BitVec-value bv2) (BitVec-value bv1))
                         (expt 2 (BitVec-width bv1)))))
          1.0))))

(define score2
  (λ (op1 op2 assignment score-bf)
    (let ([bv1 (eval op1 assignment)]
          [bv2 (eval op2 assignment)])
      (score-bf bv1 bv2))))

(define score-bool
  (λ (b assignment)
    (eval/id (get-bv assignment b))))

(define score-bool!
  (λ (b assignment)
    (- 1.0 (score-bool b assignment))))

(define score-seq
  (λ (es sf cf)
    (foldl (λ (e s) (cf s (sf e))) 0.0 es)))

(define score
  (λ (c assignment formula)
    (let ([get-bv^ ((curry get-bv) assignment)])
      (match formula
        [`⊤ 1.0]
        [`⊥ 0.0]
        [`(∨ ,es ...) (score-seq es ((curry score) c assignment) max)]
        [`(∧ ,es ...) (/ (score-seq es ((curry score) c assignment) +)
                         (length es))]
        [`(¬ (= ,op1 ,op2)) (score2 op1 op2 assignment score!=)]
        [`(= ,op1 ,op2) (score2 op1 op2 assignment ((curry score=) c))]
        [`(¬ (bvult ,op1 ,op2)) (score2 op1 op2 assignment ((curry score!<) c))]
        [`(bvult ,op1 ,op2) (score2 op1 op2 assignment ((curry score<) c))]
        [`(¬ ,b) (score-bool! b assignment)]
        [else (score-bool formula assignment)]))))

;(define assignment (hash-set (hash-set (make-immutable-hash) "a" (mkBV 3 1)) "b" (mkBV 3 2)))