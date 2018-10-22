#lang racket

(require "parser.rkt")
(require "data.rkt")

(define get-bv
  (λ (assignment sym)
    (hash-ref assignment (symbol->string sym))))


; the evaluator
(define eval
  (λ (be)
    (match be
      [`(bvadd ,op1 ,op2) (eval/bvadd (eval op1) (eval op2))]
      [`(bvsub ,op1 ,op2) (eval/bvsub (eval op1) (eval op2))]
      [`(bvmul ,op1 ,op2) (eval/bvmul (eval op1) (eval op2))]
      [`(bvudiv ,op1 ,op2) (eval/bvudiv (eval op1) (eval op2))]
      [`(bvand ,op1 ,op2) (eval/bvand (eval op1) (eval op2))]
      [`(bvor ,op1 ,op2) (eval/bvor (eval op1) (eval op2))])))

(define Hamming-distance
  (λ (bv1 bv2)
    (let ([bs1 (BitVec->bits bv1)]
          [bs2 (BitVec->bits bv2)])
      (foldl (λ (t r) (+ r (bitwise-xor (car t) (cdr t))))
             0
             (map cons bs1 bs2)))))

(define score=
  (λ (bv1 bv2)
    (if (bv= bv1 bv2)
        1.0
        (Hamming-distance bv1 bv2))))

(define score!=
  (λ (bv1 bv2)
    (if (bv= bv1 bv2)
        0.0
        1.0)))

(define score<
  (λ (bv1 bv2)
    (if (bv<)
        (error "a")
        (error "b"))))

(define score2
  (λ (op1 op2 score-bf)
    (let ([bv1 (eval op1)]
          [bv2 (eval op2)])
      (score-bf bv1 bv2))))

(define score
  (λ (c formula assignment)
    (let ([get-bv^ ((curry get-bv) assignment)])
      (match formula
        [`⊤ 1.0]
        [`⊥ 0.0]
        [`(¬ (= ,op1 ,op2)) (score2 op1 op2 score!=)]
        [`(= ,op1 ,op2) (score2 op1 op2 score=)]
        [`(bvult ,op1 ,op2) #t]
        [`(,op ...) #f]))))