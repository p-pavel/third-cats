package com.perikov.thirdcats

trait IsComposable[A <: Arrow]:
  def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom]


trait Category[A <: Arrow] extends IsComposable[A]:
  type IsObj[t]
  def id[t](using o: IsObj[t]): Arrow.Id[A,t]
