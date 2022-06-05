package com.perikov.thirdcats

import com.perikov.thirdcats.Arrow.Aux

trait ComposableBase:
  type A <: Arrow
  def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom]

trait IsComposable[B <: Arrow] extends ComposableBase:
  type A = B

trait CategoryBase extends ComposableBase:
  type IsObj[t]
  def id[t](using o: IsObj[t]): Arrow.Id[A,t]



trait Category[B <: Arrow] extends ComposableBase:
  type A = B



