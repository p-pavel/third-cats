package com.perikov.thirdcats.scala

import com.perikov.thirdcats.{Arrow, Category, Composable}

object ScalaCategory extends Category:
  type A = FuncWrapper
  opaque type IsObj[T] = Unit
  given [T]: IsObj[T] = ()
  def dom(a: A): IsObj[a.Dom] = summon
  def compose(a1: A, a2: A)(using p: a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom] =
    new FuncWrapper :
      type Dom = a2.Dom
      type Codom = a1.Codom
      val function = (a: Dom) => a1(p(a2(a)))

  def id[T](using IsObj[T]): Arrow.Aux[A,T,T] = FuncWrapper(identity)

given Composable.Aux[FuncWrapper] = ScalaCategory