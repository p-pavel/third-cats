package com.perikov.thirdcats.rec_schemes
import com.perikov.thirdcats.scala.ScalaFunctor

import scala.Function.const

trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]
def fmap[A,B, F[_]: Functor](f: A => B):  F[A] => F[B]  =  (fa: F[A]) => fa.map(f)


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

  def cata[T[_]:Functor,C](phi: Algebra[T,C]):Mu[T] => C =
    fromMu[T] >>> fmap(cata(phi)) >>> phi

  def ana[T[_]: Functor,C](psi: CoAlgebra[T,C]): C => Mu[T] =
    psi       >>> fmap(ana(psi))  >>> toMu

  def para[T[_]: Functor, C](alg: RAlgebra[T,C]): Mu[T] => C =
    fromMu[T] >>> fmap( identity[Mu[T]] &&& para(alg)) >>> alg

  def cata1[T[_]: Functor, C](phi: Algebra[T,C]): Mu[T] => C =
    def alg(t: T[(Mu[T],C)]): C = phi(t.map(_._2))
    para(alg)

  def para2[T[_]: Functor, C](alg: RAlgebra1[T,C])(t:Mu[T]): C =
    val t1 = alg(t)
    val t2 = fmap(para2(alg))
    val t3 = fromMu(t)
    t1(t2(t3))

  def cata2[T[_]: Functor, C](phi: Algebra[T,C]): Mu[T] => C =
    para2(const(phi))


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


val mRange:MendlerCoAlgebra[[t]=>>ListF[Int,t], Int] = [A] => (f: Int=>A) => (n: Int) =>
  if (n == 0) ListF.Nil
  else ListF.Cons(n, f(n-1))

def range(n: Int) = MendlerRecursions.mana(mRange)(n)


object Duality:
  import MuType.*
  trait DualT[T]:
    type Res

  given dualFuncT[A,B]: DualT[A => B] with
    type Res = B => A

  trait Dual[T](using val d: DualT[T]):
    def apply: d.Res

  val from:[T[_]]=>Mu[T]=>T[Mu[T]] = [T[_]] => (m: Mu[T]) => m.value





val t = range(1000)
/*TODO
 * paramorphism в функторальной и Mendler схемах
 * законы морфизмов
 * категорийная интерпретация
 * Comonads
 * Дуальности
 * stack safety -- переиспользовать cata(phi)?
 * связь с fix
*/
@main
def test() =
  println(mcata(listCount)(t))