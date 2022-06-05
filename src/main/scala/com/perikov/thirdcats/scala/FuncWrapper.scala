package com.perikov.thirdcats.scala

import com.perikov.thirdcats.Arrow

trait FuncWrapper extends  Any, Arrow:
  val function: Dom => Codom
  def apply(a: Dom): Codom = function(a)

object FuncWrapper:
  class FuncWrapperImpl[A,B](val function: A => B) extends AnyVal, FuncWrapper:
    type Dom = A
    type Codom = B

  inline def apply[A, B](f: A => B): Arrow.Aux[FuncWrapper, A, B] =
    FuncWrapperImpl(f)

inline given [A,B]: Conversion[A => B, Arrow.Aux[FuncWrapper,A,B]] =
  (func: A => B) => FuncWrapper(func)

inline given [A,B]: Conversion[Arrow.Aux[FuncWrapper,A,B], A => B] = _.function