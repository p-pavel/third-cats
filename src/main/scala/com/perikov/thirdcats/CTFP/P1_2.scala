package com.perikov.thirdcats.CTFP

import com.perikov.thirdcats.scala.{FuncWrapper, ScalaCategory, given}
import com.perikov.thirdcats.{Arrow, Category}


object P1_2:
  class memoizeFix[A,B](func: Rec[A,B]) extends Rec[A,B]:
    import collection.mutable
    val _cache = mutable.Map.empty[A,B]
    def apply(next: A => B): A => B = a => _cache.getOrElseUpdate(a, func(next)(a))

  type Rec[A,B] = (A => B) => A => B
  def fix[A,B](f: Rec[A,B]): A => B = a => f(fix(f))(a)

  def fact(next: Int => BigInt)(n: Int): BigInt =
    require(n >= 0)
    if n <= 1 then 1
    else n*next(n-1)

  def fib(next: Int => BigInt)(n: Int): BigInt =
    require(n > 0)
    if n <= 2 then 1
    else next(n-1) + next(n-2)

  inline transparent def runTest[A,B](name: String, func: Rec[A,B])(arg: A) =
    println(s"$name($arg)= ${fix(func)(arg)}")


  @main
  def test() =
    val number = 500
    runTest("fact",fact)(number)
    runTest("memoized",memoizeFix(fib))(number)


  object sampleCat extends Category:
    opaque type A <: Arrow = FuncWrapper
    opaque type IsObj[T] = Unit
    given [T] (using T =:= Nothing | T =:= Boolean | T =:= Unit): IsObj[T] = ()
    def id[T](using IsObj[T]): Arrow.Id[A,T] = ScalaCategory.id[T]
    def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom): Arrow.Aux[A,a2.Dom, a1.Codom] =
      ScalaCategory.compose(a1,a2)
    def arr[t1,t2](f: t1 => t2)(using IsObj[t1], IsObj[t2]): Arrow.Aux[A,t1,t2] =
      FuncWrapper(f)
    def dom(a: A):IsObj[a.Dom] = ()
    def codom(a: A):IsObj[a.Codom] = ()

  val tst = sampleCat.arr((n:Boolean) => !n)
  sampleCat.compose(tst,tst)

