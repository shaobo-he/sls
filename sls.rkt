#lang racket

(require "parsing/parse.rkt")
(require "score.rkt")
(require "data/bit-vec.rkt")
(require "data/fp.rkt")

(provide (all-defined-out))

(define get/extended-neighbors
  (λ (v)
    (match v
      [(struct BitVec _) (get/bv-extended-neighbors v)]
      [(struct FloatingPoint _) (get/fp-extended-neighbors v)]
      [_ (error "unimplemented type!")])))

(define select/Candidates
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment wp c2)
    (let* ([currScore ((score c2 assignment) F)]
           [candAssertion (select/Assertion F assignment c2)]
           [get/neighbors
            (λ (candVars)
              (apply append
                     (map
                      (λ (candVar)
                        (let ([bv (get-value assignment candVar)])
                          (map ((curry update/Assignment) assignment candVar)
                               (get/extended-neighbors bv))))
                      candVars)))]
           [select/Move
            (λ (candVars)
              (let ([neighbors (get/neighbors candVars)])
                (if (coin-flip wp)
                    (cons #t (list-ref neighbors (random (length neighbors))))
                    (cons #f (argmax
                     (λ (a) ((score c2 a) F))
                     neighbors)))))])
      (let ([local-opt (select/Move (get/vars candAssertion assignment))])
        (if (car local-opt)
            (cons #t (cdr local-opt))
        (if (> ((score c2 (cdr local-opt)) F) currScore)
            (cons #t (cdr local-opt))
            (cons #f (cdr local-opt))))))))

(define select/Assertion
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment c2)
    ;; assume when the score of an assertion is 1, it's satisfiable
    ;; no diversification TODO: UCB
    (let ([as (get/assertions F)])
      (cdr (argmax (λ (t) (let ([s ((score c2 assignment) (cdr t))])
                            (if (< s 1) s -1)))
                   (for/list ([i (in-range (length as))]
                              [a as])
                     (cons i a)))))))

(define isSat?
  (λ (F assignment c2)
    (= ((score c2 assignment) F) 1.0)))

(define update/Assignment
  (λ (assignment sym val)
    (hash-set assignment sym val)))

(define initialize/Assignment
  (λ (var-info)
    (define initialize-var
      (λ (var-name h)
        (hash-set
         h
         var-name
         (let ([type (hash-ref var-info var-name)])
           (cond
             [(bv-type? type)
              (initialize/bv (get/bv-type-width type))]
             [(fp-type? type)
              (let ([widths (get/fp-type-widths type)])
                (initialize/fp
                 (car widths)
                 (cdr widths)))]
             [(bool-type? type)
              (initialize/bv 1)])
           ))))
    (foldl
     initialize-var
     (make-immutable-hash)
     (hash-keys var-info))))
#|
(define initialize/selected
  (λ (F)
    (map (λ (x) 0) F)))

(define get/maxSteps
  (λ (c4 i)
    (if (= (modulo i 2) 0)
        (* c4 (expt 2 (/ i 2)))
        c4)))

(define sls
  (λ (F Vars c1 c2 c4)
    (let ([moves 0]
          [selected (initialize/selected F)])
      (letrec ([sls/do (λ (i)
                         (let ([assignment (initialize/assignment Vars)]
                               [maxSteps (get/maxStep c4 i)])
                           )])))
|#

(define randomAssign
  (λ (var-info assign i)
    (define initialize-var
      (λ (var-name h)
        (hash-set
         h
         var-name
         (let ([type (hash-ref var-info var-name)])
           (cond
             [(bv-type? type)
              (random/bv (get/bv-type-width type))]
             [(fp-type? type)
              (let ([widths (get/fp-type-widths type)])
                (random/fp
                 (car widths)
                 (cdr widths)))]
             [(bool-type? type)
              (random/bv 1)])
           ))))
    (begin
      (displayln "local maxima reached!")
      (displayln assign)     
      (displayln i)
      (foldl
       initialize-var
       (make-immutable-hash)
       (hash-keys var-info)))))
      ;;(let ([c (list-ref Vars (random (length Vars)))])
      ;;  (hash-set assign (symbol->string (car c)) (random-bv (cdr c)))))))

(define sls
  (λ (var-info F c2 maxSteps wp)
    (define sls/do
      (λ (i assignment)
        (if (>= i maxSteps)
            (begin
              (displayln assignment)
              'unknown)
            (if (isSat? F assignment c2)
                (begin
                  (displayln i)
                  assignment)
                (let ([newAssign (select/Candidates F assignment wp c2)])
                  (if (car newAssign)
                      (sls/do (+ i 1) (cdr newAssign))
                      (sls/do (+ i 1) (randomAssign var-info (cdr newAssign) i))
                      ))))))
    (sls/do 0 (initialize/Assignment var-info))))

;(define assignment (hash-set (hash-set (make-immutable-hash) "x" (mkBV 8 100)) "y" (mkBV 8 50)))