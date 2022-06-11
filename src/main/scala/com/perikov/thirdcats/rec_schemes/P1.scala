package com.perikov.thirdcats.rec_schemes

import com.perikov.thirdcats.scala.ScalaFunctor


trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

type MendlerAlgebra[F[_],C] = [A]=>(A => C)=>F[A] => C
type MendlerCoAlgebra[F[_],C] = [A]=>(C => A)=> C => F[A]

type Algebra[F[_],A] = F[A] => A

case class Mu[T[_]](value: T[Mu[T]]) extends AnyVal

def toMu[T[_]](t: T[Mu[T]]):Mu[T] = Mu(t)
def fromMu[T[_]](mu: Mu[T]):T[Mu[T]] = mu.value

def fmap[A,B](f: A => B): [F[_]] => Functor[F] ?=> F[A] => F[B]  = [F[_]] => (func: Functor[F]) ?=> (fa: F[A]) => fa.map(f)

def cata[T[_]:Functor,C](phi: Algebra[T,C]):Mu[T] => C =
  fromMu[T] andThen fmap(cata(phi))[T] andThen phi

def mcata[T[_],C](phi: MendlerAlgebra[T,C]): Mu[T] => C =
  fromMu[T] andThen phi(a=>mcata(phi)(a))

def mana[T[_],C](psi: MendlerCoAlgebra[T,C]):C => Mu[T] =
  psi(a=>mana(psi)(a)) andThen toMu

enum ListF[+T, +A]:
  case Nil extends ListF[Nothing,Nothing]
  case Cons(head: T, tail: A)

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
 * stack safety
 * связь с fix
*/
@main
def test() =
  println(mcata(listCount)(t))