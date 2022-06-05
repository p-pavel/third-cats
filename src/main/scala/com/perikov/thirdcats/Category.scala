package com.perikov.thirdcats


trait Composable:
  type A <: Arrow
  def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom]

object Composable:
  type Aux[t] = Composable {type A = t}

trait Category extends Composable:
  type IsObj[t]
  def id[t](using o: IsObj[t]): Arrow.Id[A,t]
  def dom(a: A): IsObj[a.Dom]
  def codom(a: A): IsObj[a.Codom]

extension [A <: Arrow](a2: A) def andThen(a1: A)(using p: a2.Codom =:= a1.Dom, c: Composable.Aux[A]):
  Arrow.Aux[A,a2.Dom, a1.Codom] = c.compose(a1,a2)


inline def compose[A <: Arrow](a1: A, a2: A)(using p: a2.Codom =:= a1.Dom, c: Composable.Aux[A]):
  Arrow.Aux[A,a2.Dom, a1.Codom] = c.compose(a1,a2)



