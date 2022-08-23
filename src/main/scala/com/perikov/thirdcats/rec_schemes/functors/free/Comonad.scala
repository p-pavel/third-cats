package com.perikov.thirdcats.rec_schemes.functors.free

import com.perikov.thirdcats.rec_schemes.functors.{Functor, Monad}

case class Cofree[F[_], A](extract: A, next: F[Cofree[F, A]])


//TODO: Comonad and zippers
trait Comonad[F[_]] extends Functor[F] :
  extension[A] (ma: M[A]) def extract: A
  extension[A] (ma: M[A]) def dup: M[M[A]]

def extend[F[_]: Comonad,A,B,C](f1: F[A]=> B, f2:F[B]=>C): F[A] => C =
  fa => f2(fa.dup.map(f1))

given cofreeIsComonad[F[_] : Functor]: Comonad[[t] =>> Cofree[F, t]] with
  extension[A] (fa: M[A]) def map[B](f: A => B): M[B] =
    fa.copy(f(fa.extract), fa.next.map(_.map(f)))
  extension[A] (ma: M[A]) def extract: A = ma.extract
  extension[A] (ma: M[A]) def dup: M[M[A]] = ??? // TODO: implement



