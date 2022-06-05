package com.perikov.thirdcats

trait Functor extends  Arrow :
  type F[_]
  type Dom <: Arrow
  type Codom <: Arrow
  def apply(arg: Dom): Arrow.Aux[Codom, F[arg.Dom], F[arg.Codom]]

type EndoFunctor[A] = Arrow.Aux[Functor,A,A]
type IdFunctor[A] = Arrow.Aux[Functor,A,A] {type F[t] = t}
trait EndoFunctorImpl[G[_], A <: Arrow] extends  Functor:
  type F[t] = G[t]
  type Dom = A
  type Codom = A

class IdFunctorImpl[A <: Arrow] extends EndoFunctorImpl[[t] =>> t, A]:
  def apply(arg: Dom) = arg

object Functor:
  type Aux[G[_], A1 <: Arrow, A2 <: Arrow] = Functor {type F[t] = G[t]; type Dom = A1; type Codom = A2}
  def id[A <: Arrow]: IdFunctor[A] = IdFunctorImpl()

inline given functorCategory: Category[Functor] with
  trait IsObj[t]:
    def idFunctor: IdFunctor[t]
  given [A <: Arrow]: IsObj[A] with
    def idFunctor = IdFunctorImpl()

  def id[t](using o: IsObj[t]): IdFunctor[t] = o.idFunctor
  def compose(a1: Functor, a2: Functor)(using proof: a2.Codom =:= a1.Dom): Arrow.Aux[Functor, a2.Dom, a1.Codom] =
    new Functor :
      self =>
      type Dom = a2.Dom
      type Codom = a1.Codom
      type F[t] = a1.F[a2.F[t]]

      def apply(arg: Dom): Arrow.Aux[Codom, F[arg.Dom], F[arg.Codom]] =
        val t1: Arrow.Aux[a2.Codom, a2.F[arg.Dom], a2.F[arg.Codom]] = a2(arg)
        val t2: Arrow.Aux[a1.Dom, a2.F[arg.Dom], a2.F[arg.Codom]] = t1.asInstanceOf
        val t3 = a1(t2)
        t3
