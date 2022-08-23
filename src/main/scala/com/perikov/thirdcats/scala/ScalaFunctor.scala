package com.perikov.thirdcats.scala

import com.perikov.thirdcats.{Functor,Arrow}

// TODO: derive
trait ScalaFunctor extends Functor :
  type Dom = FuncWrapper
  type Codom = FuncWrapper

trait ScalaFunctorAux[T[_]]:
  type F[t] = T[t]

object ScalaFunctor:
  type Aux[T[_]] = ScalaFunctor {type F[t] = T[t]}
  def apply[G[_]](f: [A,B] => (A => B) => G[A] => G[B]): ScalaFunctor {type F[t] = G[t]} =
    new ScalaFunctor:
      type F[t] = G[t]
      def apply(arg: FuncWrapper): Arrow.Aux[FuncWrapper, F[arg.Dom], F[arg.Codom]] =
        FuncWrapper(f(arg.function))
