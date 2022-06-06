package com.perikov.thirdcats.examples

import com.perikov.thirdcats.*

class FreeCategory[Original <: Arrow]:
  type A = FreeArrow[Original]
  type IsObj[T] = A#IsObj[T]
  def dom  (a: A): IsObj[a.Dom  ] = a.dom
  def codom(a: A): IsObj[a.Codom] = a.codom
  def id[T](using a: IsObj[T]): Arrow.Aux[A, T,T] = a
  def compose(a1: A, a2: A)(using a2.Codom =:= a1.Dom): Arrow.Aux[A,a2.Dom, a1.Codom] = a1 compose a2
  def arr(a: Original): Arrow.Aux[A,a.Dom, a.Codom] = Original(a).asInstanceOf

sealed trait FreeArrow[A <: Arrow] extends Any, Arrow, Matchable:
  type IsObj[T] = IdArrow[A,T]
  type Aux[t1,t2] <: Arrow.Aux[FreeArrow[A],t1,t2]
  def dom  : IsObj[Dom  ]
  def codom: IsObj[Codom]
  def compose(other: FreeArrow[A])(using other.Codom =:= Dom): Aux[ other.Dom, Codom]

trait RightComposable[A <: Arrow] extends Any, FreeArrow[A]:
  override type Aux[t1,t2] = Arrow.Aux[RightComposable[A],t1,t2]
  def extract(using Composable.Aux[A]): Aux[Dom,Codom] = ???
  def compose(other: FreeArrow[A])(using other.Codom =:= Dom): Aux[ other.Dom, Codom]

case class Original[A <: Arrow](val a: A) extends AnyVal, RightComposable[A]:
  override def dom  : IsObj[a.Dom  ] = IdArrow.left(a)
  override def codom: IsObj[a.Codom] = IdArrow.right(a)
  override type Dom   = a.Dom
  override type Codom = a.Codom
  def compose(other: FreeArrow[A])(using proof: other.Codom =:= Dom): Aux[other.Dom, Codom] =
    other match
      case p@IdArrow(_) => this.asInstanceOf[Aux[other.Dom, Codom]]
      //REPORT:  type equivalence in pattern matching
      // summon[p.Codom =:= other.Codom]
      case t: RightComposable[A] =>
        val t1: Aux[t.Dom, a.Codom] = Composition(a,t).asInstanceOf //REPORT type assignment via constructor
        t1.asInstanceOf[Aux[other.Dom, Codom]] // type equivalence in pattern matching



// TODO: Left/Right subclasses for pretty printing?
case class IdArrow[A <: Arrow,T](val a: A) extends AnyVal, FreeArrow[A]:
  override def dom  : IsObj[T] = this
  override def codom: IsObj[T] = this
  override type Dom   = T
  override type Codom = T
  type Aux[t1,t2] = Arrow.Aux[FreeArrow[A],t1,t2]
  def compose(other: FreeArrow[A])(using proof: other.Codom =:= Dom): Aux[ other.Dom, Codom] =
    val proof2: Aux[other.Dom, other.Codom] =:=  Aux[other.Dom, Codom] = proof.liftCo[[t]=>>Aux[other.Dom, t]]
    proof2(other)

object IdArrow:
  def left [A <: Arrow](a: A): IdArrow[A,a.Dom  ] = new IdArrow(a)
  def right[A <: Arrow](a: A): IdArrow[A,a.Codom] = new IdArrow(a)

case class Composition[A <: Arrow] (val a2: A, val a1: RightComposable[A])
//  (using a1.Codom =:= a2.Dom) // TODO: report this
  extends RightComposable[A]:
  override def dom  : IsObj[a1.Dom  ] = a1.dom
  override def codom: IsObj[a2.Codom] = IdArrow.right(a2)
  override type Dom   = a1.Dom
  override type Codom = a2.Codom
  def compose(other: FreeArrow[A])(using other.Codom =:= Dom): Aux[ other.Dom, Codom] =
    other match
      case _: IdArrow[A,?] => this.asInstanceOf[Aux[other.Dom, Codom]]
      case t: RightComposable[A] =>
        val t2: Arrow.Aux[RightComposable[A], other.Dom, other.Codom] = t.asInstanceOf
        val t1 = a1.compose(t2)
        Composition(a2,t1).asInstanceOf[Aux[other.Dom,Codom]]



private object TestComposision:
  sealed trait Edge extends Arrow
  case object Edge1 extends Edge {type Dom = Int; type Codom = Int}
  case object Edge2 extends Edge {type Dom = Int; type Codom = Float}
  case object Edge3 extends Edge {type Dom = Float; type Codom = Int}
  val cat = FreeCategory[Edge]()
  extension (a: Edge) def andThen(b: Edge)(using a.Codom =:= b.Dom): Arrow.Aux[cat.A, a.Dom, b.Codom] =
    cat.arr(a) andThen cat.arr(b)
  extension (a: cat.A) def andThen(b: cat.A)(using a.Codom =:= b.Dom): Arrow.Aux[cat.A, a.Dom, b.Codom] =
    cat.compose(b, a)


  val e1 = Edge1 andThen Edge2
  @main
  def test() =
    println(IdArrow.left(Edge1) andThen cat.arr(Edge1))
    println(cat.arr(Edge1) andThen IdArrow.right(Edge1))
    println(IdArrow.left(Edge1) andThen IdArrow.left(Edge1))
    val t1 = Edge1 andThen Edge1
    println(t1)
    println(t1 andThen IdArrow.right(Edge1))
    println(IdArrow.left(Edge1) andThen t1)
    println(t1 andThen t1)
    println(cat.arr(Edge1) andThen t1)
    println(t1 andThen cat.arr(Edge1))
