#lang racket

(require racket/struct)

; `value` is a positive integer whose two's complement
; is the bv to represent.
; it simplifies arithmetic
(struct BitVec (width value)
  #:constructor-name mkBV
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (bv) 'BitVec)
      (λ (bv) `(,(BitVec-width bv)
                ,(BitVec-value bv)))))])

; utils
(define BitVec->bits
  (λ (bv)
    (let ([v (BitVec-width bv)])
      (for/list ([p (range (BitVec-width bv))])
        (if (bitwise-bit-set? v p) 1 0)))))

; arithmetic operations
; binary
(define eval/arith/binop
  (λ (op bv1 bv2)
    (let ([r (op (BitVec-value bv1)
                 (BitVec-value bv2))]
          [d (expt 2 (BitVec-width bv1))])
      (mkBV (BitVec-width bv1)
            (modulo r d)))))

(define eval/bvadd
  ((curry eval/arith/binop) +))

(define eval/bvsub
  ((curry eval/arith/binop) -))

(define eval/bvmul
  ((curry eval/arith/binop) *))

(define eval/bvudiv
  ((curry eval/arith/binop) /))

(define eval/urem
  (λ (bv1 bv2)
    (error "not implemented!")))

; arithmetic negation
(define eval/bvneg
  (λ (bv)
    (let ([width (BitVec-width bv)])
      (mkBV width
            (modulo
             (- (BitVec-value bv))
             (expt 2 width))))))

; bitwise operations
; binary
(define eval/bitwise/binop
  (λ (op bv1 bv2)
    (mkBV (BitVec-width bv1) (op (BitVec-value bv1)
        (BitVec-value bv2)))))

(define eval/bvand
  ((curry eval/bitwise/binop bitwise-and)))

(define eval/bvor
  ((curry eval/bitwise/binop bitwise-ior)))

; bitwise not
(define eval/bvnot
  (λ (bv)
    (let ([v (BitVec-value bv)]
          [w (BitVec-width bv)])
      (mkBV w (- (- (expt 2 w) 1) v)))))