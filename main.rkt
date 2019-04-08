#lang racket

(provide (all-defined-out))

(require "sls.rkt"
         "parsing/parse.rkt"
         "parsing/transform.rkt"
         racket/cmdline)

(define seed (make-parameter 1))
(define c2 (make-parameter 1/2))
(define wp (make-parameter 0.001))
(define step (make-parameter 200))

(define file-to-analyze
  (command-line
   #:program "sls"
   #:once-each
   [("--seed") seed-arg
               "RNG seed"
               (define n-seed (string->number seed-arg))
               (if (and
                    n-seed
                    (exact-positive-integer? n-seed)
                    (>= n-seed 0)
                    (<= n-seed (sub1 (expt 2 31))))
                   (seed n-seed)
                   (error "not a valid seed"))]
   [("--c2") c2-arg
             "Score scaling constant"
             (define n-c2 (string->number c2-arg))
             (if (and
                  n-c2
                  (rational? n-c2)
                  (>= n-c2 0)
                  (<= n-c2 1))
                 (c2 n-c2)
                 (error "not a valid score scaling constant"))]
   [("--wp") wp-arg
             "Diversification probability"
             (define n-wp (string->number wp-arg))
             (if (and
                  n-wp
                  (>= n-wp 0)
                  (<= n-wp 1))
                 (wp n-wp)
                 (error "not a valid diversification probability"))]
   [("--step") step-arg
               "Search steps"
               (define n-step (string->number step-arg))
               (if (and
                    n-step
                    (exact-nonnegative-integer? n-step))
                   (step n-step)
                   (error "not a valid search step"))]
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(define main
  (λ ()
    (let* ([script (file->sexp file-to-analyze)]
           [formula (remove-fpconst
                     (simplify
                      (unnest
                       (formula->nnf
                        (remove-let-bindings
                         (get-formula script))))))]
           [var-info (get-var-info script)])
      (begin
        (random-seed (seed))
        (sls var-info formula (c2) (step) (wp))))))

(main)