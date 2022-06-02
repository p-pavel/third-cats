package com.perikov.thirdcats.basics

object Attemt25 {
  trait Arr:
    type Dom
    type Codom

//  trait Functor extends Arr :
//    type F[_]
//
//    def apply[T1, T2](arg: Dom {type Dom = T1; type Codom = T2}):
//    Codom {type Dom = F[T1]; type Codom = F[T2]}

  trait Composable[A <: Arr]:
    def compose(a1: A, a2: A {type Codom = a1.Dom}): A {type Dom = a2.Dom; type Codom = a1.Codom}

//    object FunctorComposable extends Composable[Functor]:
//      type A = Functor
//      def compose(
//        a1: A, a2: A {type Codom = a1.Dom}): A {type Dom = a2.Dom; type Codom = a1.Codom} =
//        new Functor:
//          type F[t] = a1.F[a2.F[t]]
//          type Dom = a2.Dom
//          type Codom = a1.Codom
//          def apply[T1,T2](arg: Dom {type Dom = T1; type Codom = T2}):
//            Codom{type Dom = F[T1]; type Codom = F[T2]} =
//              a1(a2(arg))

    trait Nat[C1 <: Arr, C2 <: Arr] extends Arr
//      type F1[_]
//      type F2[_]
//      type Dom = Functor {type Dom = C1; type Codom = C2; type F[t] = F1[t]}
//      type Codom = Functor {type Dom = C1; type Codom = C2; type F[t] = F2[t]}
//      def apply[T]: C2 {type Dom = F1[T]; type Codom = F2[T]}

    class NatComposable[C1 <: Arr, C2 <: Arr](using comp: Composable[C2]) extends Composable[Nat[C1,C2]]:
      def compose(a1: Nat[C1,C2], a2: Nat[C1,C2] {type Codom = a1.Dom}): Nat[C1,C2] {type Dom = a2.Dom; type Codom = a1.Codom} =
        new Nat[C1,C2] {type Dom = a2.Dom; type Codom = a1.Codom}
//          type F1[t] = a2.F1[t]
//          type F2[t] = a1.F2[t]
//          def apply[T]: C2 {type Dom = F1[T]; type Codom = F2[T]} = ???


}