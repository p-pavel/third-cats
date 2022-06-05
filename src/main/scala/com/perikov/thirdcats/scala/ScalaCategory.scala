package com.perikov.thirdcats.scala

import com.perikov.thirdcats.{Arrow, IsComposable}

object ScalaCategory extends IsComposable[FuncWrapper] :
  type A = FuncWrapper

  def compose(a1: A, a2: A)(using p: a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom] =
    new FuncWrapper :
      type Dom = a2.Dom
      type Codom = a1.Codom
      val function = (a: Dom) => a1(p(a2(a)))

given IsComposable[FuncWrapper] = ScalaCategory