package com.perikov.thirdcats.rec_schemes.functors

trait Pointed[F[_]] extends Functor[F] :
  def pure[A](a: A): M[A]
