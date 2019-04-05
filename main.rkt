#lang racket

(provide (all-defined-out))

(require "sls.rkt"
         "parsing/parse.rkt"
         "parsing/transform.rkt")

(define main
  (λ ()
    (define args (current-command-line-arguments))
    (let* ([script (file->sexp (vector-ref args 0))]
           [formula (remove-fpconst
                     (simplify
                      (unnest
                       (formula->nnf
                        (remove-let-bindings
                         (get-formula script))))))]
           [var-info (get-var-info script)]
           [seed (string->number (vector-ref args 1))])
      (begin
        (random-seed seed)
        (sls var-info formula 1/2 200 0.0001)))))

(main)