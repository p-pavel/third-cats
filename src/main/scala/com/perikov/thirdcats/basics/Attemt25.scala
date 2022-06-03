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
    type Dom = Arr.Aux[Functor, C1,C2]
    type Codom = Arr.Aux[Functor,C1,C2]
    def apply[T]: Arr.Aux[C2, self.Dom#F[T], self.Codom#F[T] ]

  trait Composable[ A <: Arr] :
    def compose(a1: A , a2: A ) (using a2.Codom =:= a1.Dom): Arr.Aux[A, a2.Dom, a1.Codom]



  class composableNat[c1 <: Arr, c2 <: Arr](using comp: Composable[c2]) extends Composable[ Nat[c1,c2]]:
    type A = Nat[c1,c2]
    def compose(a1: A , a2: A ) (using proof: a2.Codom =:= a1.Dom):
    A {type Dom = a2.Dom; type Codom = a1.Codom} =
      new Nat[c1,c2]:
        self=>
        def apply[T]: c2{type Dom = self.Dom#F[T]; type Codom = self.Codom#F[T] } =
          val t2: Arr.Aux[c2,a2.Dom#F[T], a2.Codom#F[T]] = a2[T]
          val t1: Arr.Aux[c2,a1.Dom#F[T], a1.Codom#F[T]] = a1[T]
          val postulateToWorkAroundTypeSystemLimitation: a2.Codom#F[T] =:= a1.Dom#F[T] = proof.asInstanceOf
          val tst = postulateToWorkAroundTypeSystemLimitation.liftCo[[q]=>>Arr.Aux[c2,a2.Dom#F[T], q]]
          val t3: Aux[c2, a2.Codom#F[T], a1.Dom#F[T]] = tst(t2)
          val t4  = comp.compose(t1,t3)
          t4

  inline given composableNat[c1 <: Arr, c2 <: Arr](using c:Composable[c2]): Composable[Nat[c1,c2]] = new composableNat[c1,c2]

  trait FuncWrapper extends Arr{
    val function: Dom => Codom
    def apply(a: Dom): Codom = function(a)
  }
  object FuncWrapper:
    inline def apply[A,B](f: A=>B):Arr.Aux[FuncWrapper,A,B] =
      new FuncWrapper:
        override val function = f
        override type Dom = A
        override type Codom = B

  object ScalaCategory extends Composable[FuncWrapper]:
    type A = FuncWrapper
    def compose(a1: A , a2: A ) (using p:a2.Codom =:= a1.Dom): Arr.Aux[A, a2.Dom, a1.Codom] =
      new FuncWrapper:
        type Dom = a2.Dom
        type Codom = a1.Codom
        val function = (a: Dom) => a1(p(a2(a)))

  given Composable[FuncWrapper] = ScalaCategory

  given [A,B]: Conversion[A => B, Arr.Aux[FuncWrapper,A,B]] =
    (func: A => B) => new FuncWrapper:
      override val function = func
      override type Dom = A
      override type Codom = B

  trait ScalaFunctor extends Functor:
    type Dom = FuncWrapper
    type Codom = FuncWrapper

  def idFunctor[A <: Arr]: Functor {type F[t] = t; type Dom = A; type Codom = A} =
    new Functor:
      type F[t] = t
      type Dom = A
      type Codom = A
      def apply(arg: A):Arr.Aux[A,arg.Dom, arg.Codom] = arg

  def compose[A <: Arr](a1: A, a2: A)(using c: Composable[A], eq: a2.Codom =:= a1.Dom): Arr.Aux[A, a2.Dom, a1.Codom] =
    c.compose(a1,a2)

  object ListFunctor extends ScalaFunctor:
    type F[t] = List[t]
    def apply[A,B](f: A => B): List[A] => List[B] = _.map(f)

  object OptionFunctor extends ScalaFunctor:
    type F[t] = Some[t]
    def apply[A,B](f: A => B): Option[A] => Option[B] = _.map(f)

  val ScalaIdFunctor = idFunctor[FuncWrapper]
  type ScalaNat = Nat[FuncWrapper, FuncWrapper]

//  object id2List extends Nat[FuncWrapper,FuncWrapper]
}