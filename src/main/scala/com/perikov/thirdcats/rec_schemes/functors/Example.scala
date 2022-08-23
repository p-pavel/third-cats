package com.perikov.thirdcats.rec_schemes.functors

type Algebra[F[_],A] = F[A] => A

import com.perikov.thirdcats.rec_schemes.MuType.*
type RAlgebra[F[_],A] = Mu[F]=>F[A] => A


