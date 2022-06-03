package com.perikov.thirdcats.basics

import com.perikov.thirdcats.basics.Attemt25.Arr.Aux

object Attemt25 {
  trait Arr:
    type Dom
    type Codom

  object Arr:
    type Aux[A <: Arr, t1, t2] = A {type Dom = t1; type Codom = t2}

  trait Functor extends Arr :
    type F[_]
    type Dom <: Arr
    type Codom <: Arr

  object Functor:
    type Aux[G[_],A1 <: Arr, A2 <: Arr] = Functor {type F[t] = G[t]; type Dom = A1; type Codom = A2}

  trait Nat[C1 <: Arr, C2 <: Arr] extends Arr:
    self=>
    type F1[_]
    type F2[_]
    type Dom = Functor.Aux[F1,C1,C2]
    type Codom = Functor.Aux[F2,C1,C2]
    def apply[T]: Arr.Aux[C2, self.Dom#F[T], self.Codom#F[T] ]

  trait Composable[ A <: Arr] :
    def compose(a1: A , a2: A ) (using a2.Codom =:= a1.Dom): Arr.Aux[A, a2.Dom, a1.Codom]



  class composableNat[c1 <: Arr, c2 <: Arr](using comp: Composable[c2]) extends Composable[ Nat[c1,c2]]:
    type A = Nat[c1,c2]
    def compose(a1: A , a2: A ) (using proof: a2.Codom =:= a1.Dom):
    A {type Dom = a2.Dom; type Codom = a1.Codom} =
      new Nat[c1,c2]:
        self=>
        type F1[t] = a2.F1[t]
        type F2[t] = a1.F2[t]
        def apply[T]: c2{type Dom = self.Dom#F[T]; type Codom = self.Codom#F[T] } =
          val t2: Arr.Aux[c2,a2.Dom#F[T], a2.Codom#F[T]] = a2[T]
          val t1: Arr.Aux[c2,a1.Dom#F[T], a1.Codom#F[T]] = a1[T]
          val postulateToWorkAroundTypeSystemLimitation: a2.Codom#F[T] =:= a1.Dom#F[T] = proof.asInstanceOf
          val tst = postulateToWorkAroundTypeSystemLimitation.liftCo[[q]=>>Arr.Aux[c2,a2.Dom#F[T], q]]
          val t3: Aux[c2, a2.F1[T], a1.F1[T]] = tst(t2)
          val t4  = comp.compose(t1,t3)
          t4

  inline given composableNat[c1 <: Arr, c2 <: Arr](using c:Composable[c2]): Composable[Nat[c1,c2]] = new composableNat[c1,c2]
}