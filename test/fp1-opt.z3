; Start decls (5)
(declare-fun |c::main::main::1::IN@1!0&0#0| () (_ FloatingPoint 8 24))
(declare-fun |c::main::main::1::result@1!0&0#1| () (_ FloatingPoint 8 24))
(declare-fun |c::main::main::1::x@1!0&0#1| () (_ FloatingPoint 8 24))
(declare-fun |execution_statet::guard_exec@0!0| () Bool)
(declare-fun |goto_symex::guard@0!0&0#1| () Bool)
; End decls
; Start constraints (7)
(assert (= |c::main::main::1::IN@1!0&0#0| |c::main::main::1::x@1!0&0#1|))
(assert (let ((a!1 (fp.neg (fp.div roundNearestTiesToEven
                           (fp.mul roundNearestTiesToEven
                                   (fp.mul roundNearestTiesToEven
                                           |c::main::main::1::x@1!0&0#1|
                                           |c::main::main::1::x@1!0&0#1|)
                                   |c::main::main::1::x@1!0&0#1|)
                           (fp #b0 #x81 #b10000000000000000000000))))
      (a!2 (fp.mul roundNearestTiesToEven
                   (fp.mul roundNearestTiesToEven
                           (fp.mul roundNearestTiesToEven
                                   (fp.mul roundNearestTiesToEven
                                           |c::main::main::1::x@1!0&0#1|
                                           |c::main::main::1::x@1!0&0#1|)
                                   |c::main::main::1::x@1!0&0#1|)
                           |c::main::main::1::x@1!0&0#1|)
                   |c::main::main::1::x@1!0&0#1|)))
(let ((a!3 (fp.add roundNearestTiesToEven
                   (fp.add roundNearestTiesToEven
                           (fp.add roundNearestTiesToEven
                                   |c::main::main::1::x@1!0&0#1|
                                   a!1)
                           (fp.div roundNearestTiesToEven
                                   a!2
                                   (fp #b0 #x85 #b11100000000000000000000)))
                   (fp.div roundNearestTiesToEven
                           (fp.mul roundNearestTiesToEven
                                   (fp.mul roundNearestTiesToEven
                                           a!2
                                           |c::main::main::1::x@1!0&0#1|)
                                   |c::main::main::1::x@1!0&0#1|)
                           (fp #b0 #x8b #b00111011000000000000000)))))
  (= a!3 |c::main::main::1::result@1!0&0#1|))))
(assert (not (and (fp.leq |c::main::main::1::result@1!0&0#1|
                  (fp #b0 #x7f #b00000010100011110101110))
          (fp.leq (fp #b1 #x7f #b00000010100011110101110)
                  |c::main::main::1::result@1!0&0#1|))))
(assert (fp.lt (fp #b1 #x7f #b10010010000111111011011) |c::main::main::1::IN@1!0&0#0|))
(assert (fp.lt |c::main::main::1::IN@1!0&0#0| (fp #b0 #x7f #b10010010000111111011011)))
(assert |execution_statet::guard_exec@0!0|)
(assert |goto_symex::guard@0!0&0#1|)
; End constraints
