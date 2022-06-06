package com.perikov.thirdcats

trait EmptyCategory extends Category:
  opaque type IsObj[T] = Nothing
  trait A extends Arrow:
    val forbidden: Nothing //Can't make type A = Nothing https://github.com/lampepfl/dotty/issues/15377
  override def dom(a: A): IsObj[A] = a.forbidden
  override def codom(a: A): IsObj[A] = a.forbidden
  override def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom ): Arrow.Aux[A,a2.Dom, a1.Codom] = a1.forbidden
  override def id[T](using o: IsObj[T]): Nothing = o


