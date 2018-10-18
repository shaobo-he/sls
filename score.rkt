#lang racket

(require "parser.rkt")
(require "data.rkt")

(define get-bv
  (λ (assignment sym)
    (hash-ref assignment (symbol->string sym))))

(define Hamming-distance
  (λ (bv1 bv2)
    (let ([bs1 (BitVec->bits bv1)]
          [bs2 (BitVec->bits bv2)])
      (foldl (λ (t r) (+ r (bitwise-xor (car t) (cdr t))))
             0
             (map cons bs1 bs2)))))

(define score
  (λ (c formula assignment)
    (let ([get-bv^ ((curry get-bv) assignment)])
    (match formula
      [`⊤ 1.0]
      [`⊥ 0.0]
      [`(= ,op1 ,op2) (let ([bv1 (get-bv^ op1)]
                            [bv2 (get-bv^ op2)])
                        ) ]
      [`(bvult ,op1 ,op2) #t]
      [`(,op ...) #f])