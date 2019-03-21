#lang racket
(require math/bigfloat)
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
      (reverse (for/list ([p (range (BitVec-width bv))])
        (if (bitwise-bit-set? v p) 1 0))))))

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

(define bv≤
  ((curry bv-pred) <=))

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

(define get/1-exchange
  (λ (bv)
    (let ([v (BitVec-value bv)]
          [w (BitVec-width bv)])
      (for/list ([i (in-range w)])
        (let ([mask (arithmetic-shift 1 i)])
          (mkBV w (bitwise-xor mask v)))))))

(define get/±1
  (λ (bv)
    (let ([w (BitVec-width bv)])
      (for/list ([f `(,eval/bvadd ,eval/bvsub)])
        (f bv (mkBV w 1))))))

(define get/extended-neighbors
  (λ (bv)
    (append (get/1-exchange bv) (get/±1 bv))))
    ;(get/1-exchange bv)))

(define coin-flip
  (λ (p)
    (if (< (random) p)
        #t
        #f)))

(define random-bv
  (λ (w)
    (mkBV w (random (expt 2 w)))))
    ;(mkBV w (random 7))))

;; sig-width includes the hidden bit
(struct FloatingPoint (exp-width sig-width value)
  #:constructor-name mkFP
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (fp) 'FloatingPoint)
      (λ (fp) `(,(FloatingPoint-exp-width fp)
                ,(FloatingPoint-sig-width fp)
                ,(FloatingPoint-value fp)))))])

(define fp/infinity?
  (λ (fp)
    (bfinfinite? (FloatingPoint-value fp))))

(define fp/nan?
  (λ (fp)
    (bfnan? (FloatingPoint-value fp))))

(define real->FloatingPoint
  (λ (rv exp-width sig-width)
    (parameterize ([bf-precision sig-width])
      (mkFP exp-width sig-width (bfcopy rv)))))

(define BitVec->FloatingPoint
  (λ (bv exp-width sig-width)
    (mkFP exp-width sig-width
          (if (= (BitVec-width bv) (+ exp-width sig-width))
              (parameterize ([bf-precision sig-width])
                (let* ([bv-value (BitVec-value bv)]
                       [sig-width-wo (- sig-width 1)]
                       [sig-bits (modulo bv-value (expt 2 sig-width-wo))]
                       [exp-bits (modulo (arithmetic-shift bv-value (- 0 sig-width-wo))
                                         (expt 2 exp-width))]
                       [exp-bias (- (expt 2 (- exp-width 1)) 1)]
                       [sign-bit (bitwise-bit-set? bv-value (+ exp-width sig-width-wo))])
                  (cond
                    [(= (- (expt 2 exp-width) 1) exp-bits) ; exp all 1s
                     (if (= sig-bits 0)
                         (bfcopy (if sign-bit -inf.bf +inf.bf))
                         (bfcopy +nan.bf))]
                    [(= exp-bits 0) ; exp all 0s
                     (if (= sig-bits 0)
                         (bfcopy (if sign-bit -0.bf 0.bf))
                         (bf (if sign-bit (- 0 sig-bits) sig-bits)
                             (- (+ (- 0 exp-bias) 1) sig-width-wo)))]
                    [else (bf (let ([sig (+ (expt 2 sig-width-wo) sig-bits)])
                                (if sign-bit (- 0 sig) sig))
                              (- (- exp-bits exp-bias)
                                 sig-width-wo))])))
              (error "Bit width doesn't match!")))))

(define FloatingPoint->BitVec
  (λ (fp)
    (let* ([exp-width (FloatingPoint-exp-width fp)]
           [sig-width (FloatingPoint-sig-width fp)]
           [fp-val (FloatingPoint-value fp)]
           [sig-width-wo (- sig-width 1)]
           [exp-bias (- (expt 2 (- exp-width 1)) 1)]
           [sign-wrap (λ (v)
                        (if (bfnegative? fp-val)
                            (+ v (expt 2 (+ exp-width sig-width-wo)))
                            v))])
      (mkBV
       (+ exp-width sig-width)
       (cond
         [(fp/nan? fp) (error "no unique bv representation for nans!")]
         [(fp/infinity? fp) (sign-wrap (arithmetic-shift (- (expt 2 exp-width) 1) sig-width-wo))]
         [else
          (let-values ([(sig exp) (bigfloat->sig+exp fp-val)])
            (sign-wrap
             (if (and (>= sig (expt 2 sig-width-wo))
                      (< sig (expt 2 sig-width)))
                 (if (>= exp (- (- 1 exp-bias) sig-width-wo)) ; normals
                     (+
                      (arithmetic-shift (- exp (- (- 0 exp-bias) sig-width)) (- sig-width-wo 1))
                      (modulo sig (expt 2 (- sig-width 1))))
                     (/ sig (expt 2 (- (- 1 (+ exp-bias sig-width-wo)) exp))))
                 (error "unrecognized format for sig+exp!"))))])))))

