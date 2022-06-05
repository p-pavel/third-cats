package com.perikov.thirdcats

trait Arrow extends Any:
  type Dom
  type Codom

object Arrow:
  type Aux[A <: Arrow, t1, t2] = A {type Dom = t1; type Codom = t2}
  type Id[A <: Arrow, t] = Aux[A,t,t]

