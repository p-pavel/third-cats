package com.perikov.thirdcats.rec_schemes.functors

trait Functor[F[_]]:
  type M[t] = F[t]
  extension[A] (fa: M[A]) def map[B](f: A => B): M[B]

def fmap[F[_]:Functor,A,B](f: A => B): F[A] => F[B] = _.map(f)
def fmap2[A,B](f: A => B): [F[_]] => Functor[F] ?=> F[A] => F[B] = [F[_]] => (func:Functor[F]) ?=> (fa: F[A]) =>
  fa.map(f)


type ~>[F[_],G[_]] = [A] => F[A] => G[A]