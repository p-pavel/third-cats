package com.perikov

type Arr = {
  type Dom
  type Codom
}

object Arr:
  type Aux[Obj <: Arr,A,B] = Obj {type Dom = A; type Codom = B}
  def mark[T[_,_], A, B](a: T[A,B]): T[A,B] & Arr  = a.asInstanceOf

trait ArrTrait extends Any{
  type Dom
  type Codom
}


trait Category:
  type IsObject[_]
  type Arrow <: Arr
  type HomSet[A,B] = Arrow {type Dom = A; type Codom = B}
  def compose(f: Arrow, g: HomSet[?, f.Dom] ): HomSet[g.Dom,f.Codom]
  def id[A](using IsObject[A]): HomSet[A,A]
  def arrowIndexedByObjects(arr: Arrow): (IsObject[arr.Dom], IsObject[arr.Codom])

object Category:
  type Aux[isObj[_], arr <: Arr] = Category {type IsObject[t]=isObj[t]; type Arrow = arr}


object ScalaCat extends Category:
  opaque type IsObject[T] = Unit
  inline given [T]: IsObject[T] = ()
  trait Arrow extends ArrTrait:
    val function: Dom => Codom
  class ArrImpl[A,B](val function: A => B) extends Function[A,B],Arrow:
    type Dom = A; type Codom = B
    def apply(a:A): B = function(a)
  inline given [A,B]: Conversion[A => B, ArrImpl[A,B]] = ArrImpl(_)
  def compose(f: Arrow, g: HomSet[?, f.Dom] ): HomSet[g.Dom,f.Codom] = ArrImpl(g.function andThen f.function)
  def id[A](using IsObject[A]): HomSet[A,A] = ArrImpl(identity)
  def arrowIndexedByObjects(arr: Arrow): (IsObject[arr.Dom], IsObject[arr.Codom]) = (summon,summon)

trait TypeFunc:
  type F[_]

trait Functor extends ArrTrait, TypeFunc:
  type Dom <: Arr
  type Codom <: Arr
  def fmap(arg: Dom): Arr.Aux[Codom, F[arg.Dom],F[arg.Codom]]
object Functor {
  type Aux[G[_],A,B] = Arr.Aux[Functor,A,B] {type F[t] = G[t]}
}

type Id[t] = t
object CategoryOfCategories extends Category:
  type IdFunctor[A ] = Functor.Aux[Id, A, A]
  sealed trait IsObject[A]:
    def id: IdFunctor[A]

  given [T <: Arr]: IsObject[T] with
    def id = new Functor {
      type Dom = T; type Codom = T; type F[t] = t
      def fmap(arg: Dom): Arr.Aux[Codom, F[arg.Dom],F[arg.Codom]] = arg
    }

  type Arrow = Functor
  def compose(f: Arrow, g: HomSet[?, f.Dom] ): HomSet[g.Dom,f.Codom] = new Functor {
    type Dom = g.Dom
    type Codom = f.Codom
    type F[t] = f.F[g.F[t]]
    def fmap(arg: Dom): Arr.Aux[Codom, F[arg.Dom],F[arg.Codom]] = f.fmap(g.fmap(arg))
  }
  def id[A](using o: IsObject[A]): IdFunctor[A] = o.id
  def arrowIndexedByObjects(arr: Arrow): (IsObject[arr.Dom], IsObject[arr.Codom]) = (summon, summon)

trait IsFunctor[F[_],]

//class NatHom (val srcCat: Category, val dstCat: Category) extends Category:
//  type FuncAux[G[_]] = Functor.Aux[G, srcCat.Arrow, dstCat.Arrow]
//  trait Arrow extends ArrTrait:
//    type F1[_]
//    type F2[_]
//    type Dom <: FuncAux[F1]
//    type Codom <: FuncAux[F2]
//    def apply[A](using srcCat.IsObject[A]): dstCat.HomSet[F1[A],F2[A]]
//
//  type Aux[f1[_],g1[_], func1 <: FuncAux[f1], func2: FuncAux[f2]
//
//  sealed trait IsObject[A]
//  object IsObject:
//    def apply[G[_], Func <: FuncAux[G]]=
//      new IsObject[Func]:
//        def id = new Arrow:
//          type F1[t] = G[t]
//          type F2[t] = G[t]
//          type Dom = Func
//          type Codom = Func
//          def apply[A](using srcCat.IsObject[A]): dstCat.HomSet[F1[A],F2[A]] =
//
//  type Aux[f1[_],f2[_]] = Arrow {type F1[t] = f1[t]; type F2[t] = f2[t]}
//  type IdNat[F[_]] = Aux[F,F]
//  def compose(f: Arrow, g: HomSet[?, f.Dom] ): HomSet[g.Dom,f.Codom] = ???
//  def id[A](using IsObject[A]): HomSet[A,A] = ???
//  def arrowIndexedByObjects(arr: Arrow): (IsObject[arr.Dom], IsObject[arr.Codom]) = ???
//
//TODO: ScalaFunctor
//TODO: ScalaNat