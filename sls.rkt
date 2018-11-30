#lang racket

(require "parser.rkt")
(require "score.rkt")
(require "data.rkt")

#|
(define select/Candidates
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment wp)
    (let ([a (select/Assertion F assignment)])
      (get/vars a))))
|#

(define select/Assertion
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment c2)
    ;; assume when the score of an assertion is 1, it's satisfiable
    ;; no diversification TODO: UCB
    (let ([as (get/assertions F)])
      (car (argmax (λ (t) (let ([s (score c2 assignment (cdr t))])
                            (if (< s 1.0) s -1.0)))
                   (for/list ([i (in-range (length as))]
                              [a as])
                     (cons i a)))))))

#|
(define initialize/assignment
  (λ (Vars)
    ))

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

(define assignment (hash-set (hash-set (make-immutable-hash) "x" (mkBV 8 10)) "y" (mkBV 8 5)))