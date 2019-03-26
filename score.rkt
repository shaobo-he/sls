#lang racket

(require "data/bit-vec.rkt")
(require "data/fp.rkt")

(provide (all-defined-out))

(define get-value
  (λ (assignment sym)
    (hash-ref assignment sym)))

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
                        [`(_ ,op1, op2) (mkBV op2 (string->number
                                               (substring
                                                (symbol->string op1)
                                                (string-length "bv"))))]
                        [`(fp.add ,rm ,op1 ,op2) (eval/fpadd (eval^ op1) (eval^ op2))]
                        [`(fp.sub ,rm ,op1 ,op2) (eval/fpsub (eval^ op1) (eval^ op2))]
                        [`(fp.mul ,rm ,op1 ,op2) (eval/fpmul (eval^ op1) (eval^ op2))]
                        [`(fp.div ,rm ,op1 ,op2) (eval/fpdiv (eval^ op1) (eval^ op2))]
                        [`(fp.isNormal ,op) (mkBoolBV (fp/normal? (eval^ op)))]
                        [`(fp.isSubnormal ,op) (mkBoolBV (fp/subnormal? (eval^ op)))]
                        [`(fp.isZero ,op) (mkBoolBV (fp/zero? (eval^ op)))]
                        [`(fp.isPositive, op) (mkBoolBV (fp/positive? (eval^ op)))]
                        [(struct FloatingPoint _) be]
                        [(struct BitVec _) be]
                        [`(,op ...) (error "unsupported operations")]
                        [else (get-value assignment be)]))])
      (eval^ be))))

(define Hamming-distance
  (λ (bv1 bv2)
    (let ([bs1 (BitVec->bits bv1)]
          [bs2 (BitVec->bits bv2)])
      (foldl (λ (t r) (+ r (bitwise-xor (car t) (cdr t))))
             0
             (map cons bs1 bs2)))))

; smt equality/inequality
; bv's equality
(define (score/bv= c bv1 bv2)
  (if (bv= bv1 bv2)
      1.0
      (* c (- 1.0
              (/ (Hamming-distance bv1 bv2)
                 (BitVec-width bv1))))))
; fp's equality
(define (score/fp= c fp1 fp2)
  (cond
    [(and (fp/nan? fp1) (fp/nan? fp2)) 1.0]
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0.0]
    [else
     (score/bv=
      c
      (FloatingPoint->BitVec fp1)
      (FloatingPoint->BitVec fp2))]))

(define ((score/= c) v1 v2)
  (match v1
    [(struct BitVec _) (score/bv= c v1 v2)]
    [(struct FloatingPoint _) (score/fp= c v1 v2)]
    [_ (error "unimplemented type")]))

; bv's inequality
(define score/bv!=
  (λ (bv1 bv2)
    (if (bv= bv1 bv2)
        0.0
        1.0)))
; fp's inequality
(define score/fp!=
  (λ (fp1 fp2)
    (cond
      [(and (fp/nan? fp1) (fp/nan? fp2)) 0.0]
      [(or (fp/nan? fp1) (fp/nan? fp2)) 1.0]
      [else
       (score/bv!=
        (FloatingPoint->BitVec fp1)
        (FloatingPoint->BitVec fp2))])))

(define (score/!= v1 v2)
  (match v1
    [(struct BitVec _) (score/bv!= v1 v2)]
    [(struct FloatingPoint _) (score/fp!= v1 v2)]
    [_ (error "unimplemented type")]))

; fp's equality/inequality
(define ((score/fpeq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) #f]
    [else (score/fp= c fp1 fp2)]))

(define (score/fp!eq fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) #t]
    [else (score/bv!=
           (FloatingPoint->BitVec fp1)
           (FloatingPoint->BitVec fp2))]))

; bv's lt
(define ((score/bv< c) bv1 bv2)
  (if (bv< bv1 bv2)
      1.0
      (*
       c
       (-
        1.0
        (/
         (- (BitVec-value bv1) (BitVec-value bv2))
         (expt 2 (BitVec-width bv1)))))))

; bv's geq
(define ((score/bv!< c) bv1 bv2)
  (if (bv< bv1 bv2)
      (*
       c
       (-
        1.0
        (/
         (- (BitVec-value bv2) (BitVec-value bv1))
         (expt 2 (BitVec-width bv1)))))
      1.0))

; fp's lt

(define get/fp-pos
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (if (fp/positive? fp)
        (BitVec-value (FloatingPoint->BitVec fp))
        (- 0
           (-
            (BitVec-value (FloatingPoint->BitVec fp))
            (expt 2 (- (+ exp-width sig-width) 1))
            )))))

(define ((score/fp< c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0.0]
    [(fp< fp1 fp2) 1.0]
    ;; fp1 >= fp2 and fp1 != nan and fp2 != nan
    [else
     (*
      c
      (-
       1.0
       / (- (get/fp-pos fp1) (get/fp-pos fp2))
       (expt
        2
        (+ 1
           (+
            (FloatingPoint-exp-width fp1)
            (FloatingPoint-sig-width fp2))
           ))))]))

(define ((score/fp!< c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1.0]
    [(fp≥ fp1 fp2) 1.0]
    ;; fp1 < fp2 and fp1 != nan and fp2 != nan
    [else
     (*
      c
      (-
       1.0
       / (- (get/fp-pos fp2) (get/fp-pos fp1))
       (expt
        2
        (+ 1
           (+
            (FloatingPoint-exp-width fp1)
            (FloatingPoint-sig-width fp2))
           ))))]))

(define score2
  (λ (op1 op2 assignment score-bf)
    (let ([bv1 (eval op1 assignment)]
          [bv2 (eval op2 assignment)])
      (score-bf bv1 bv2))))

; bool's score function
; note that bool is treated as bv1
(define score-bool
  (λ (v)
    (eval/id v)))

(define score-bool!
  (λ (v)
    (- 1.0 (score-bool v))))

(define score-seq
  (λ (es sf cf)
    (foldl (λ (e s) (cf s (sf e))) 0.0 es)))

(define ((score c assignment) formula)
  (let ([get-value^ ((curry get-value) assignment)])
    (match formula
      [`⊤ 1.0]
      [`⊥ 0.0]
      [`(∨ ,es ...) (score-seq es (score c assignment) max)]
      [`(∧ ,es ...) (/ (score-seq es (score c assignment) +)
                       (length es))]
      [`(¬ (= ,op1 ,op2)) (score2 op1 op2 assignment score/!=)]
      [`(= ,op1 ,op2) (score2 op1 op2 assignment (score/= c))]
      [`(¬ (bvult ,op1 ,op2)) (score2 op1 op2 assignment (score/bv!< c))]
      [`(bvult ,op1 ,op2) (score2 op1 op2 assignment (score/bv< c))]
      [`(¬ (fp.lt ,op1 ,op2)) (score2 op1 op2 assignment (score/fp!< c))]
      [`(fp.lt ,op1 ,op2) (score2 op1 op2 assignment (score/fp< c))]
      [`(¬ ,b) (score-bool! (eval b assignment))]
      [else (score-bool (eval formula assignment))])))

;(define assignment (hash-set (hash-set (make-immutable-hash) "a" (mkBV 3 1)) "b" (mkBV 3 2)))