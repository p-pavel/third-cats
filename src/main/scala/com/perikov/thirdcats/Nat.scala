package com.perikov.thirdcats


class NatCategory(val c1: CategoryBase, val c2: CategoryBase) extends CategoryBase:
  trait Nat extends Arrow:
    type F1[_]
    type F2[_]
    type Dom = Functor.Aux[F1,c1.A, c2.A]
    type Codom = Functor.Aux[F2, c1.A, c2.A]
    def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, Dom#F[T], Codom#F[T]]

  type A = Nat

  trait IsObj[T]:
    type F[_]
    def idNat: Arrow.Id[Nat, T]

  object IsObj:
    def apply[G[_]](func: Functor.Aux[G,c1.A,c2.A]) =
      new IsObj[Functor.Aux[G,c1.A,c2.A]]:
        type F[t] = G[t]
        def idNat: Arrow.Id[Nat,Functor.Aux[F,c1.A, c2.A]] =
          new Nat:
            type F1[t] = F[t]
            type F2[t] = F[t]
            def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, F1[T], F2[T]] =
              func(c1.id[T])

  def id[T](using obj: IsObj[T]): Arrow.Id[Nat,T] = obj.idNat
  def id(func: Arrow.Aux[Functor,c1.A,c2.A]): Arrow.Id[Nat, Functor.Aux[func.F,c1.A, c2.A] ] =
    id(using IsObj(func))

  def compose(a1: A, a2: A)(using proof: a2.Codom =:= a1.Dom): Arrow.Aux[A, a2.Dom, a1.Codom] =
    new Nat:
      self=>
      type F1[t] = a2.F1[t]
      type F2[t] = a1.F2[t]
      def apply[T](using obj: c1.IsObj[T]): Arrow.Aux[c2.A, self.Dom#F[T], self.Codom#F[T]] =
        val t2: Arrow.Aux[c2.A,a2.Dom#F[T], a2.Codom#F[T]] = a2[T]
        val t1: Arrow.Aux[c2.A,a1.Dom#F[T], a1.Codom#F[T]] = a1[T]
        val postulateToWorkAroundTypeSystemLimitation: a2.Codom#F[T] =:= a1.Dom#F[T] = proof.asInstanceOf
        val tst = postulateToWorkAroundTypeSystemLimitation.liftCo[[q]=>>Arrow.Aux[c2.A,a2.Dom#F[T], q]]
        val t3: Arrow.Aux[c2.A, a2.Dom#F[T], a1.Dom#F[T]] = tst(t2)
        val t4: Arrow.Aux[c2.A, a2.Dom#F[T], a1.Codom#F[T]] = c2.compose(t1,t3)
        t4
