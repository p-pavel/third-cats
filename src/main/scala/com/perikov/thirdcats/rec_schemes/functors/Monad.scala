package com.perikov.thirdcats.rec_schemes.functors


trait Monad[F[_]] extends Pointed[F] :
  extension[A] (mma: M[M[A]]) def join: M[A]
  extension[A] (ma: M[A]) def flatMap[B](f: A => M[B]): M[B] =
    ma.map(f).join
  extension [A](fa: F[A]) def ap[B](f: F[A => B]): F[B] =
    for
      ff <- f
      aa <- fa
    yield ff(aa)


object Monad:
  def apply[M[_]: Monad] = summon[Monad[M]]