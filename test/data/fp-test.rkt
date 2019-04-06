#lang racket

(require rackunit
         rackunit/text-ui
         "../../data/fp.rkt")

(define file-tests
  (test-suite
   "Tests for Floating-point Data Types"

   (test-case
    "Creation of floating-point values"
    (check-pred
     fp/infinity?
     (real->FloatingPoint 65520.0 5 11))
    (check-pred
     fp/positive?
     (real->FloatingPoint 0.0 5 11))
    (check-pred
     fp/negative?
     (real->FloatingPoint -0.0 5 11)))
   
   (test-case
    "Floating-point to bit-vector conversions")

   (test-case
    "Floating-point arithmetic")
   ))

(run-tests file-tests)