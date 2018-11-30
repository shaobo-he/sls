#lang racket

(require "parser.rkt")
(require "score.rkt")
(require "data.rkt")


(define select/Candidates
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment wp c2)
    (let* ([currScore (score c2 assignment F)]
           [candAssertion (select/Assertion F assignment c2)]
           [get/neighbors
            (λ (candVars)
              (apply append
                     (map
                      (λ (candVar)
                        (let ([bv (get-bv assignment candVar)])
                          (map ((curry update/Assignment) assignment candVar)
                               (get/1-exchange bv))))
                      candVars)))]
           [select/Move
            (λ (candVars)
              (let ([neighbors (get/neighbors candVars)])
                (if (coin-flip wp)
                    (list-ref neighbors (random (length neighbors)))
                    (argmax
                     (λ (a) (score c2 a F))
                     neighbors))))])
      (let ([local-opt (select/Move (get/vars candAssertion assignment))])
        (if (> (score c2 local-opt F) currScore)
            (cons #t local-opt)
            (cons #f local-opt))))))

(define coin-flip
  (λ (p)
    (if (< (random) p)
        #t
        #f)))

(define select/Assertion
  ;;(λ (F assignment selected moves c2)
  (λ (F assignment c2)
    ;; assume when the score of an assertion is 1, it's satisfiable
    ;; no diversification TODO: UCB
    (let ([as (get/assertions F)])
      (cdr (argmax (λ (t) (let ([s (score c2 assignment (cdr t))])
                            (if (< s 1.0) s -1.0)))
                   (for/list ([i (in-range (length as))]
                              [a as])
                     (cons i a)))))))

(define isSat?
  (λ (F assignment c2)
    (= (score c2 assignment F) 1.0)))

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

(define randomAssign
  (λ (assign i)
    (begin
      (displayln assign)
      (displayln i)
      (error "local maxima reached!"))))

(define sls
  (λ (F c2 initAssign maxSteps wp)
    (letrec ([sls/do
              (λ (i assignment)
                (if (>= i maxSteps)
                    'unknown
                    (if (isSat? F assignment c2)
                        assignment
                        (let ([newAssign (select/Candidates F assignment wp c2)])
                          (if (car newAssign)
                              (sls/do (+ i 1) (cdr newAssign))
                              (sls/do (+ i 1) (randomAssign (cdr newAssign) i))
                              )))))])
      (sls/do 0 initAssign))))

(define assignment (hash-set (hash-set (make-immutable-hash) "x" (mkBV 8 100)) "y" (mkBV 8 50)))