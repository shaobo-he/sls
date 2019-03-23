#lang racket

(require "../../parsing/parse.rkt")
(require "../../parsing/transform.rkt")

(define test-script
  "
   (declare-const x\n
   (_ BitVec 8))\n
   (declare-const y (_ BitVec 8))\n
   (assert (or (bvult (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))\n
               (= (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))))\n
   (assert (or (bvult y (_ bv2 8))\n
               (= y (_ bv2 8))))\n
   (assert (not (bvult (bvadd x y) (_ bv1 8))))")

#|
(define let-script "(assert
 (not (fp.isNaN ((_ to_fp 11 53) a_ackermann!0))))
(assert
 (let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
 (let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
 (not (fp.eq ?x14 ((_ to_fp 11 53) (_ bv0 64)))))))
(assert
 (let ((?x30 (fp.add roundNearestTiesToEven (fp.mul roundNearestTiesToEven ((_ to_fp 11 53) a_ackermann!0) ((_ to_fp 11 53) (_ bv4652104065864433664 64))) ((_ to_fp 11 53) (_ bv4638321995473517281 64)))))
(let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
(let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
(let ((?x22 (fp.add roundNearestTiesToEven ((_ to_fp 11 53) (_ bv0 64)) (fp.mul roundNearestTiesToEven ?x14 ((_ to_fp 11 53) (_ bv4652104065864433664 64))))))
(let ((?x28 (fp.add roundNearestTiesToEven (fp.add roundNearestTiesToEven ?x22 ((_ to_fp 11 53) (_ bv4638321995473517281 64))) ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))
(not (fp.eq ?x28 (fp.add roundNearestTiesToEven ?x30 ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))))))))
")
|#
(define let-script "(assert                                                                         
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))                                  
 (fp.geq ?x8 ((_ to_fp 11 53) (_ bv4487126258331716666 64)))))                  
(assert                                                                         
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))                                  
 (fp.leq ?x8 ((_ to_fp 11 53) (_ bv4487186704622697397 64)))))
(assert                                                                         
 (let ((?x52 ((_ to_fp 11 53) (_ bv4607182418800017408 64))))                   
 (let ((?x41 (bvadd (_ bv4293918720 32) (bvor (bvand (bvand ((_ extract 63 32) fresh_to_ieee_bv_!2) (_ bv2147483647 32)) (_ b
v1048575 32)) (_ bv1072693248 32)))))
 (let ((?x45 (concat ((_ extract 15 8) ?x41) (concat ((_ extract 7 0) ?x41) ((_ extract 31 0) fresh_to_ieee_bv_!1)))))
 (let ((?x50 ((_ to_fp 11 53) (concat ((_ extract 31 24) ?x41) (concat ((_ extract 23 16) ?x41) ?x45)))))
 (let ((?x55 (fp.div roundNearestTiesToEven ?x52 (fp.add roundNearestTiesToEven ?x50 ?x52))))
 (let ((?x53 (fp.sub roundNearestTiesToEven ?x50 ?x52)))                        
 (let ((?x56 (fp.mul roundNearestTiesToEven ?x53 ?x55)))                        
 (= ?x56 ((_ to_fp 11 53) fresh_to_ieee_bv_!3))))))))))
")

(define sexp (string->sexp let-script))
;(remove-let-bindings sexp)

(define test-formula (unnest (formula->nnf (get-formula (string->sexp test-script)))))
(define test-vars (get-vars (string->sexp test-script)))
(define kenken-formula (unnest (formula->nnf (get-formula (file->sexp "../test3.smt2")))))
(define kenken-vars (get-vars (file->sexp "../test3.smt2")))

(define fp-test-formula (file->sexp "../fp1.z3"))

