package com.perikov
type Id[A] = A
type Compose[F[_],G[_]] = [A] =>> F[G[A]]
trait Arr:
  type Dom
  type Codom
object Arr:
  type Aux[T<: Arr, A,B] = T{type Dom = A; type Codom = B}

type Obj = { type Repr}
object Obj:
  type Aux[O <: Obj,R] = O {type Repr = R}

trait Composable:
  type A <: Arr
  def compose(f: A, g: Arr.Aux[A,? ,f.Dom]): Arr.Aux[A,g.Dom,f.Codom]

object Composable {
  type Aux[a <: Arr] = Composable {type A = a}
}

trait Category extends Composable:
  type O <: Obj
  def id[T](using Obj.Aux[O,T]): Arr.Aux[A,T,T]

object Category:
  type Aux[o <: Obj,a <: Arr] = Category { type O = o; type A=a}

trait FunctorBase extends Arr:
  self =>
  type Dom   <: Arr
  type Codom <: Arr
  type F[_]

sealed trait Functor extends FunctorBase:
  self =>
  def fmap(arg: Dom): Arr.Aux[Codom, F[arg.Dom],F[arg.Codom]]
  def andThen(other: Arr.Aux[Functor, Codom, ?] ) =
    Functor[Compose[other.F,F], self.Dom, other.Codom]((a: Dom) => other.fmap(self.fmap(a)))
//sealed trait ContraFunctor extends FunctorBase:
//  def fmap(arg: srcCat.A): Arr.Aux[dstCat.A, F[arg.Codom],F[arg.Dom]]


object Functor:
  type Aux[G[_], c1 <: Arr, c2 <: Arr] = Functor {type F[t] = G[t]; type Dom = c1; type Codom = c2}
  def id[A <: Arr] = Functor[Id,A,A]((a: A) => a)
  transparent inline def apply[G[_], A1 <: Arr, A2 <: Arr](func: (arg: A1) => Arr.Aux[A2,G[arg.Dom], G[arg.Codom]] ):
    Functor.Aux[G, A1, A2] =
    new Functor:
      type Dom = A1
      type Codom = A2
      type F[t] = G[t]
      def fmap(arg: Dom): Arr.Aux[Codom, F[arg.Dom],F[arg.Codom]] = func(arg)

type TypeFunc = {type F[_]}
object TypeFunc:
  type Aux[G[_]] = TypeFunc {type F[t] = G[t]}


trait Natural[O <: Obj, A <: Arr] :
  self =>
  protected val proof: Composable.Aux[A]
  type F1[_]
  type F2[_]
  type Dom = TypeFunc.Aux[F1]
  type Codom = TypeFunc.Aux[F2]
  def apply[T](using o: Obj.Aux[O,T]): Arr.Aux[A, F1[T], F2[T]]
  def andThen( other: Natural[O,A] {type F1[t] = self.F2[t]} ): Natural.Aux[O,A,self.F1, other.F2]


object Natural:
  def id[O <: Obj,A <: Arr, f[_]]: Natural.Aux[O,A,f,f] =
    ???
  type Aux[O <: Obj, A <: Arr, f1[_], f2[_]] = Natural[O,A] {type F1[t] = f1[t]; type F2[t] = f2[t]}
  def apply[O <: Obj, A <: Arr: Composable.Aux,f1[_],f2[_]](trans: [T]=> (o: Obj.Aux[O,T]) ?=> Arr.Aux[A,f1[T],f2[T]]):
    Natural.Aux[O,A,f1,f2]
    =
    new Natural[O,A]:
      self =>
      protected val proof: Composable.Aux[A] = summon
      type F1[t] = f1[t]
      type F2[t] = f2[t]
      def apply[T](using o: Obj.Aux[O,T]): Arr.Aux[A, F1[T], F2[T]] = trans[T]
      def andThen( other: Natural[O,A] {type F1[t] = self.F2[t]} ): Natural.Aux[O,A,self.F1, other.F2] =
        Natural[O,A,F1, other.F2]([T]=>(o: Obj.Aux[O,T]) ?=> {
          val t2: Arr.Aux[A, F2[T], other.F2[T]] = other[T]
          proof.compose(t2, self[T])
        })
