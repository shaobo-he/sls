#lang racket

(require math/bigfloat)
(require racket/struct)
(require "bit-vec.rkt")

(provide (all-defined-out))

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

;; floating-point predicates
(define ((fp/pred pred) fp)
  (pred (FloatingPoint-value fp)))

;; floating-point arithmetic
(define fp/result-infinity?
  (λ (v exp-width sig-width)
    (define pv (bfabs v))
    (bf>
     pv
     (FloatingPoint-value
      (get/maximum-normal exp-width sig-width)))))

(define get/+inf
  (λ (exp-width sig-width)
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
      (bfcopy +inf.bf)))))

(define get/-inf
  (λ (exp-width sig-width)
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
      (bfcopy -inf.bf)))))

(define get/maximum-normal
  (λ (exp-width sig-width)
    (define +inf-bv (FloatingPoint->BitVec (get/+inf exp-width sig-width)))
    (BitVec->FloatingPoint
     (mkBV
      (+ exp-width sig-width)
      (- (BitVec-value +inf-bv) 1))
      exp-width
      sig-width)))

(define ((eval/fparith/binop op) fp1 fp2)
  (define v1 (FloatingPoint-value fp1))
  (define v2 (FloatingPoint-value fp2))
  (define exp-w-1 (FloatingPoint-exp-width fp1))
  (define exp-w-2 (FloatingPoint-exp-width fp2))
  (define sig-w-1 (FloatingPoint-sig-width fp1))
  (define sig-w-2 (FloatingPoint-sig-width fp2))
  (if (and
       (=
        (bigfloat-precision v1)
        (bigfloat-precision v2))
       (and
        (= exp-w-1 exp-w-2)
        (= sig-w-1 sig-w-2)))
      ;; apply arithmetic
      (let ([result (parameterize ([bf-precision sig-w-1])
                      (op v1 v2))])
        (if (fp/result-infinity? result exp-w-1 sig-w-1)
            (if (bfpositive? result)
                (get/+inf exp-w-1 sig-w-1)
                (get/-inf exp-w-1 sig-w-1))
            (mkFP exp-w-1 sig-w-1 result)))
      (error "invalid arithmetic operation")))

(define eval/fpadd (eval/fparith/binop bf+))
(define eval/fpsub (eval/fparith/binop bf-))
(define eval/fpmul (eval/fparith/binop bf*))
(define eval/fpdiv (eval/fparith/binop bf/))


(define fp/infinity? (fp/pred bfinfinite?))

(define fp/nan? (fp/pred bfnan?))

(define fp/zero? (fp/pred bfzero?))

(define fp/negative? (fp/pred bfnegative?))

;; floating-point related conversions
;; real->fp
(define real->FloatingPoint
  (λ (rv exp-width sig-width)
    (parameterize ([bf-precision sig-width])
      (mkFP exp-width sig-width (bf rv)))))

;; bv->fp
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

; fp->bv
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
         [(fp/zero? fp) (sign-wrap 0)]
         [else
          (let-values ([(sig exp) (bigfloat->sig+exp fp-val)])
            (sign-wrap
             (let ([sig (abs sig)])
               (if (and (>= sig (expt 2 sig-width-wo))
                        (< sig (expt 2 sig-width)))
                   (if (>= exp (- (- 1 exp-bias) sig-width-wo)) ; normals
                       (+
                        (arithmetic-shift (+ (+ exp exp-bias) sig-width-wo) sig-width-wo)
                        (modulo sig (expt 2 (- sig-width 1))))
                       (/ sig (expt 2 (- (- 1 (+ exp-bias sig-width-wo)) exp))))
                   (error "unrecognized format for sig+exp!")))))])))))