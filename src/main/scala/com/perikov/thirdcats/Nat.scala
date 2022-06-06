package com.perikov.thirdcats


class NatCategory(val c1: Category, val c2: Category) extends Category:
  trait Nat extends Arrow:
    val dom: Arrow.Aux[Functor,c1.A, c2.A]
    val codom: Arrow.Aux[Functor,c1.A,c2.A]
    type Dom = Functor.Aux[dom.F,c1.A, c2.A]
    type Codom = Functor.Aux[codom.F, c1.A, c2.A]
    def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, Dom#F[T], Codom#F[T]]

  type A = Nat

  def dom(a: A): IsObj[a.Dom] = IsObj(a.dom)
  def codom(a: A): IsObj[a.Codom] = IsObj(a.codom)
  sealed trait IsObj[T]:
    def idNat: Arrow.Id[Nat, T]

  object IsObj:
    def apply[G[_]](func: Functor.Aux[G,c1.A,c2.A]) =
      new IsObj[Functor.Aux[G,c1.A,c2.A]]: // TODO: singleton type?
        def idNat: Arrow.Id[Nat,Functor.Aux[G,c1.A,c2.A]] =
          new Nat:
            val dom:func.type = func
            val codom:func.type = func
            def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, dom.F[T], codom.F[T]] =
              dom(c1.id[T])

  def id[T](using obj: IsObj[T]): Arrow.Id[Nat,T] = obj.idNat
  def id(func: Arrow.Aux[Functor,c1.A,c2.A]): Arrow.Id[Nat, Functor.Aux[func.F,c1.A, c2.A] ] =
    id(using IsObj(func))

  def compose(a1: A, a2: A)(using proof: a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom] =
    new Nat:
      self=>
      val dom:a2.dom.type = a2.dom
      val codom: a1.codom.type = a1.codom
      def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, self.Dom#F[T], self.Codom#F[T]] =
        val t2: Arrow.Aux[c2.A,a2.Dom#F[T], a2.Codom#F[T]] = a2[T]
        val t1: Arrow.Aux[c2.A,a1.Dom#F[T], a1.Codom#F[T]] = a1[T]
        //TODO: extract to postulate
        val postulateToWorkAroundTypeSystemLimitation: a2.Codom#F[T] =:= a1.Dom#F[T] = proof.asInstanceOf
        val tst = postulateToWorkAroundTypeSystemLimitation.liftCo[[q]=>>Arrow.Aux[c2.A,a2.Dom#F[T], q]]
        val t3: Arrow.Aux[c2.A, a2.Dom#F[T], a1.Dom#F[T]] = tst(t2)
        val t4: Arrow.Aux[c2.A, a2.Dom#F[T], a1.Codom#F[T]] = c2.compose(t1,t3)
        t4
