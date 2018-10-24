#lang racket

(require racket/struct)

(provide (all-defined-out))

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
    (let ([v (BitVec-value bv)])
      (for/list ([p (range (BitVec-width bv))])
        (if (bitwise-bit-set? v p) 1 0)))))

(define eval/id
  (curry BitVec-value))

; arithmetic operations
; binary
(define eval/arith/binop
  (λ (op bv1 bv2)
    (let ([r (op (BitVec-value bv1)
                 (BitVec-value bv2))]
          [d (expt 2 (BitVec-width bv1))])
      (mkBV (BitVec-width bv1)
            (modulo r d)))))

(define bv-pred
  (λ (op bv1 bv2)
    (if (= (BitVec-width bv1)
           (BitVec-width bv2))
        (op (BitVec-value bv1)
            (BitVec-value bv2))
        (error "not the same bit-width"))))

(define bv=
  ((curry bv-pred) =))

(define bv<
  ((curry bv-pred) <))

(define eval/bvadd
  ((curry eval/arith/binop) +))

(define eval/bvsub
  ((curry eval/arith/binop) -))

(define eval/bvmul
  ((curry eval/arith/binop) *))

(define eval/bvudiv
  ((curry eval/arith/binop) /))

(define eval/bvurem
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

(define eval/bvshl
  (λ (bv1 bv2)
    (let ([w (BitVec-width bv1)])
      (mkBV w (modulo
               (arithmetic-shift (BitVec-value bv1)
                                 (BitVec-value bv2))
               (expt 2 w))))))

(define eval/bvlshr
  (λ (bv1 bv2)
    (mkBV (BitVec-width bv1)
          (arithmetic-shift (BitVec-value bv1)
                            (- (BitVec-value bv2))))))

; bitwise not
(define eval/bvnot
  (λ (bv)
    (let ([v (BitVec-value bv)]
          [w (BitVec-width bv)])
      (mkBV w (- (- (expt 2 w) 1) v)))))
