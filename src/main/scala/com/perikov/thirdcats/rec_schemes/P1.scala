package com.perikov.thirdcats.rec_schemes
import functors.{Functor,fmap}

import scala.Function.const

object MuType:
  case class Mu[T[_]](value: T[Mu[T]]) extends AnyVal
  def toMu  [T[_]](t: T[Mu[T]]):Mu[T]    = Mu(t)
  def fromMu[T[_]](mu: Mu[T])  :T[Mu[T]] = mu.value
  type Mu1[T[_,_], A] = Mu[[t]=>>T[A, t]]


object FunctorialRecursion:
  import MuType.*
  import FuncExtensions.*
  type Algebra  [F[_],A] = F[A] => A
  type CoAlgebra[F[_],A] = A => F[A]
  type RAlgebra[F[_], A] = F[(Mu[F], A)] => A
  type RAlgebra1[F[_], A] = Mu[F] =>  F[A] => A
  type RCoAlgebra[F[_],A] = A => F[Either[Mu[F],A]]

  def cata[T[_]:Functor,C](phi: Algebra[T,C])(arg: Mu[T]): C =
    (fromMu[T] >>> fmap(a => cata(phi)(a)) >>> phi)( arg)

  def ana[T[_]: Functor,C](psi: CoAlgebra[T,C]): C => Mu[T] =
    psi       >>> fmap(a => ana(psi)(using summon)(a))  >>> toMu

  def para[T[_]: Functor, C](alg: RAlgebra[T,C]): Mu[T] => C =
    fromMu[T] >>> fmap( identity[Mu[T]] &&& para(alg)) >>> alg

  def cata1[T[_]: Functor, C](phi: Algebra[T,C]): Mu[T] => C =
    def alg(t: T[(Mu[T],C)]): C = phi(t.map(_._2))
    para(alg)

  def para1[T[_]: Functor, C](alg: RAlgebra1[T,C])(t:Mu[T]): C =
    val t1 = alg(t)
    val t2 = fmap(para1(alg))
    val t3 = fromMu(t)
    t1(t2(t3))

  def cata2[T[_]: Functor, C](phi: Algebra[T,C]): Mu[T] => C =
    para1(const(phi))

  def apo[T[_]: Functor, C](apo: RCoAlgebra[T,C]): C => Mu[T] = ???



object MendlerRecursions:
  import MuType.*
  type MendlerAlgebra  [F[_],C] = [A]=>(A => C)=> (F[A] => C   )
  type MendlerCoAlgebra[F[_],C] = [A]=>(C => A)=> (C    => F[A])

  def mcata[T[_],C](phi: MendlerAlgebra[T,C]): Mu[T] => C =
    fromMu[T] andThen phi(a=>mcata(phi)(a))

  def mana[T[_],C](psi: MendlerCoAlgebra[T,C]):C => Mu[T] =
    psi(a=>mana(psi)(a)) andThen toMu

enum ListF[+T, +A]:
  case Nil extends ListF[Nothing,Nothing]
  case Cons(head: T, tail: A)

given listFFunctor[A]:Functor[[t]=>>ListF[A,t]] with
  type F[t] = ListF[A,t]
  extension [A](fa: F[A]) def map[B](f: A => B): F[B] =
    fa match
      case ListF.Nil          => ListF.Nil
      case ListF.Cons(a,tail) => ListF.Cons(a, f(tail))

import MendlerRecursions.*
def listCount[T]: MendlerAlgebra[[t]=>>ListF[T,t],Int] = [A]=> (f: A => Int) => (l:ListF[T,A]) =>
  l match
    case ListF.Nil => 0
    case ListF.Cons(_, t) => 1 + f(t)


val mRange:CoAlgebra[[t]=>>ListF[Int,t], Int] =  (n: Int) =>
  if (n == 0) ListF.Nil
  else ListF.Cons(n, n-1)

def range = FunctorialRecursion.ana(mRange)

import MuType.*
import FunctorialRecursion.*
def filterListF[A](cond: A => Boolean): Algebra[[t]=>>ListF[A,t], Mu1[ListF,A] ] = {
  case t@ListF.Cons(head, _) if cond(head) => Mu(t)
  case ListF.Cons(_, tail) => tail
  case _ => Mu(ListF.Nil)
}

def filter1[A](cond: A => Boolean): Mu1[ListF,A] => Mu1[ListF,A] =  cata(filterListF(cond))

def toListF[A]:Algebra[[t]=>>ListF[A,List[A]], List[A]] = l =>
  l match
    case ListF.Nil => Nil
    case ListF.Cons(head, tail) => head :: tail
def toList[A]: Mu1[ListF,A] => List[A] = cata(toListF)



val t: Mu1[ListF,Int] = range(100)
/*TODO
 * законы морфизмов
 * категорийная интерпретация
 * Comonads
 * Дуальности
 * stack safety -- переиспользовать cata(phi)?
 * связь с fix
*/
@main
def test() =
  println(toList(t))
  println(toList(filter1((_: Int) % 2 == 0)(t)))