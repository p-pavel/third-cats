package com.perikov.thirdcats.rec_schemes

object Duality:

  import MuType.*

  trait DualT[T]:
    type Res

  given dualFuncT[A, B]: DualT[A => B] with
    type Res = B => A

  trait Dual[T](using val d: DualT[T]):
    def apply: d.Res

  val from: [T[_]] => Mu[T] => T[Mu[T]] = [T[_]] => (m: Mu[T]) => m.value
