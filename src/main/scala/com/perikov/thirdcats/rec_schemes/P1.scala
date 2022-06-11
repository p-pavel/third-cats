package com.perikov.thirdcats.rec_schemes
import com.perikov.thirdcats.scala.ScalaFunctor

trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

type MendlerAlgebra  [F[_],C] = [A]=>(A => C)=> (F[A] => C   )
type MendlerCoAlgebra[F[_],C] = [A]=>(C => A)=> (C    => F[A])

type Algebra  [F[_],A] = F[A] => A
type CoAlgebra[F[_],A] = A => F[A]

object FuncExtensions:
  extension [A,B,C](f: A => B) def >>> (g: B => C): A => C = f andThen g
  extension [A,B,C](f: B => C) def <<< (g: A => B): A => C = g andThen f
  extension [A,B,C](f: A => B) def &&& (g: A => C): A => (B, C) = a => (f(a),g(a))

import FuncExtensions.*

object MuType:
  case class Mu[T[_]](value: T[Mu[T]]) extends AnyVal
  def toMu  [T[_]](t: T[Mu[T]]):Mu[T]    = Mu(t)
  def fromMu[T[_]](mu: Mu[T])  :T[Mu[T]] = mu.value

import MuType.*

def fmap[A,B, F[_]: Functor](f: A => B):  F[A] => F[B]  =  (fa: F[A]) => fa.map(f)

object FunctorialRecursion:
  def cata[T[_]:Functor,C](phi: Algebra[T,C]):Mu[T] => C =
    fromMu[T] >>> fmap(cata(phi)) >>> phi

  def ana[T[_]: Functor,C](psi: CoAlgebra[T,C]): C => Mu[T] =
    psi       >>> fmap(ana(psi))  >>> toMu

object YCombinator:
  def fix[A,B](f: (A => B) => (A => B)): A => B =
    f(a => fix(f)(a))

  def factorialStep(next: Int => BigInt)(n: Int): BigInt =
    if n == 0 then 1
    else n * next(n-1)

  val factorial = fix(factorialStep)

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

def listCount[T]: MendlerAlgebra[[t]=>>ListF[T,t],Int] = [A]=> (f: A => Int) => (l:ListF[T,A]) =>
  l match
    case ListF.Nil => 0
    case ListF.Cons(_, t) => 1 + f(t)

type Mu1[T[_,_], A] = Mu[[t]=>>T[A, t]]

val mRange:MendlerCoAlgebra[[t]=>>ListF[Int,t], Int] = [A] => (f: Int=>A) => (n: Int) =>
  if (n == 0) ListF.Nil
  else ListF.Cons(n, f(n-1))

def range(n: Int) = mana(mRange)(n)

val t = range(1000)
/*TODO
 * законы морфизмов
 * категорийная интерпретация
 * Comonads
 * stack safety -- переиспользовать cata(phi)?
 * связь с fix
*/
@main
def test() =
  println(mcata(listCount)(t))