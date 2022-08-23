package com.perikov.thirdcats.rec_schemes.functors

trait Applicative[F[_]] extends Pointed[F] :
  extension[A] (fa: F[A]) def ap[B](f: F[A => B]): F[B]

inline def ap[F[_]:Applicative, A,B](func: F[A => B]): F[A] => F[B] = _.ap(func)
