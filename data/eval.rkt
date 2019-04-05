#lang racket

(require "bit-vec.rkt"
         "fp.rkt")

(provide (all-defined-out))

(define get-value
  (λ (assignment sym)
    (hash-ref assignment sym)))

; the evaluator
(define eval
  (λ (be assignment)
    (define eval^
      (λ (be)
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
          [`(_ ,op1, op2) (mkBV op2 (string->number
                                     (substring
                                      (symbol->string op1)
                                      (string-length "bv"))))]
          [`(fp.add ,rm ,op1 ,op2) (eval/fpadd (eval^ op1) (eval^ op2))]
          [`(fp.sub ,rm ,op1 ,op2) (eval/fpsub (eval^ op1) (eval^ op2))]
          [`(fp.mul ,rm ,op1 ,op2) (eval/fpmul (eval^ op1) (eval^ op2))]
          [`(fp.div ,rm ,op1 ,op2) (eval/fpdiv (eval^ op1) (eval^ op2))]
          [`(fp.neg, op) (eval/fpneg (eval^ op))]
          [`(fp.isNormal ,op) (mkBoolBV (fp/normal? (eval^ op)))]
          [`(fp.isSubnormal ,op) (mkBoolBV (fp/subnormal? (eval^ op)))]
          [`(fp.isZero ,op) (mkBoolBV (fp/zero? (eval^ op)))]
          [`(fp.isPositive, op) (mkBoolBV (fp/positive? (eval^ op)))]
          [`(fp.isNaN, op) (mkBoolBV (fp/nan? (eval^ op)))]
          [`(fp.isInfinite, op) (mkBoolBV (fp/infinity? (eval^ op)))]
          [`((_ to_fp ,new-exp-width ,new-sig-width) ,rm ,op) (eval/fpconv (eval^ op) new-exp-width new-sig-width)]
          [(struct FloatingPoint _) be]
          [(struct BitVec _) be]
          [`(,op ...) ((displayln op) (error "unsupported operations"))]
          [else (get-value assignment be)])))
    (eval^ be)))