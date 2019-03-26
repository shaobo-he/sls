; Start decls (1)
(declare-fun a () (_ FloatingPoint 8 24))
; End decls
; Start constraints (3)
(assert (or (fp.isNormal a) (fp.isZero a) (fp.isSubnormal a)))
(assert (fp.isPositive a))
(assert (not (fp.lt (fp.mul roundNearestTiesToEven
                    a
                    (fp #b0 #x7e #b10000000000000000000000))
            a)))
; End constraints
