package com.perikov

trait Arr:
  type Dom
  type Codom
object Arr:
  type Aux[T<: Arr, A,B] = T{type Dom = A; type Codom = B}

type Obj = { type Repr}
object Obj:
  type Aux[O <: Obj,R] = O {type Repr = R}

trait Category:
  type O <: Obj
  type A <: Arr
  def compose(f: A, g: Arr.Aux[A,? ,f.Dom]): Arr.Aux[A,g.Dom,f.Codom]
  def id[T](using Obj.Aux[O,T]): Arr.Aux[A,T,T]

object Category:
  type Aux[o <: Obj,a <: Arr] = Category { type O = o; type A=a}

trait Intercategory:
  val srcCat: Category
  val dstCat: Category

trait FunctorBase extends Intercategory, Arr:
  type Dom   = srcCat.type
  type Codom = dstCat.type
  type F[_]


sealed trait Functor extends FunctorBase:
  def fmap(arg: srcCat.A): Arr.Aux[dstCat.A, F[arg.Dom],F[arg.Codom]]

sealed trait ContraFunctor extends FunctorBase:
  def fmap(arg: srcCat.A): Arr.Aux[dstCat.A, F[arg.Codom],F[arg.Dom]]


object Functor:
  type Aux[G[_], c1 <: Category, c2 <: Category] = Functor {type F[t] = G[t]; type Dom = c1; type Codom = c2}
  def apply[G[_]](c1: Category, c2: Category, func: (arg: c1.A) => Arr.Aux[c2.A,G[arg.Dom], G[arg.Codom]] ):
    Functor.Aux[G,c1.type, c2.type ] =
    new Functor:
      val srcCat:c1.type = c1
      val dstCat:c2.type = c2
      type F[t] = G[t]
      def fmap(arg: srcCat.A): Arr.Aux[dstCat.A, F[arg.Dom],F[arg.Codom]] = func(arg)


trait Natural extends Intercategory:
  val dom  : Functor
  val codom: Functor
  type Dom   =  dom.type
  type Codom = codom.type
  def apply[T](using o: Obj.Aux[srcCat.O,T]): Arr.Aux[dstCat.A, dom.F[T], codom.F[T]]

object Natural:
  def apply(
            func1: Functor,
            func2: Arr.Aux[Functor,func1.Dom, func1.Codom],
            trans: [T] => (o: Obj.Aux[func1.srcCat.O, T]) ?=> Arr.Aux[func1.dstCat.A, func1.F[T], func2.F[T]]
           ) =
    new Natural:
      val srcCat: func1.srcCat.type = func1.srcCat
      val dstCat: func1.dstCat.type  = func1.dstCat
      val dom: func1.type   = func1
      val codom: func2.type = func2
      def apply[T](using o: Obj.Aux[srcCat.O,T]): Arr.Aux[dstCat.A, dom.F[T], codom.F[T]] = trans(using o)




