package com.perikov.thirdcats.rec_schemes

object YCombinatorSpike:
  def fix[A, B](f: (A => B) => (A => B)): A => B =
    f(a => fix(f)(a))

  def factorialStep(next: Int => BigInt)(n: Int): BigInt =
    if n == 0 then 1
    else n * next(n - 1)

  val factorial = fix(factorialStep)
