package com.perikov.thirdcats.rec_schemes.functors.free
import com.perikov.thirdcats.rec_schemes.functors
import functors.Functor

enum Monad[F[_], A]:
  case Pure(a: A)
  case Dup(inner: F[Monad[F, A]])

given freeMonadIsMonad[F[_] : Functor]: functors.Monad[[t] =>> Monad[F, t]] with
  extension[A] (fa: M[A]) def map[B](f: A => B): M[B] =
    fa match
      case Monad.Pure(a) => Monad.Pure(f(a))
      case Monad.Dup(inner) => Monad.Dup(inner.map(_.map(f)))

  def pure[A](a: A): M[A] = Monad.Pure(a)

  extension[A] (mma: M[M[A]]) def join: M[A] =
    mma match
      case Monad.Pure(t) => t
      //TODO: look deeper?
      case Monad.Dup(t) => Monad.Dup(t.map(_.join))

import cats.free.Free


enum Free2[T[_],A]:
  case Pure(a: A)
  case Suspend(ta: T[A])
  case FlatMap[S[_],A,B](a: Free2[S,A], f: A => Free2[S,B]) extends Free2[S,B]

trait Category:
  type IsObject <: {type T}
  type Arrow[_,_]
  def id(a: IsObject): Arrow[a.T, a.T]

object EmptyCategory extends Category:
  type IsObject = Nothing

  //compiles
  def id(a: IsObject): Nothing = a
  // this version breaks
  // def id(a: Nothing) = a