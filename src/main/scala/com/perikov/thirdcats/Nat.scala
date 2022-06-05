package com.perikov.thirdcats

trait Nat[C1 <: Arrow, C2 <: Arrow] extends Arrow :
  self =>
  type F1[_]
  type F2[_]
  type Dom = Functor.Aux[F1, C1, C2]
  type Codom = Functor.Aux[F2, C1, C2]
  def apply[T]: Arrow.Aux[C2, self.Dom#F[T], self.Codom#F[T]]
  
given natCategory[c1 <: Arrow, c2 <: Arrow] (using c1Cat: Category[c1], c2Cat:Category[c2]): Category[Nat[c1,c2]]
  with

  type A = Nat[c1,c2]
  trait IsObj[t]:
    def idNat: Arrow.Id[A,t]
  object IsObj:
    def apply[G[_]](func: Functor.Aux[G,c1,c2]): IsObj[Functor.Aux[G,c1,c2]]=
      new IsObj[Functor.Aux[G,c1,c2]]:
        def idNat: Arrow.Id[A, Functor.Aux[G,c1,c2]] =
          new Nat[c1,c2]:
            type F1[t] = G[t]
            type F2[t] = G[t]
            def apply[T]: Arrow.Aux[c2, G[T], G[T]] = ???


  def id[t](using o: IsObj[t]): Arrow.Id[A,t] = o.idNat

  def compose(a1: A , a2: A ) (using proof: a2.Codom =:= a1.Dom):
  A {type Dom = a2.Dom; type Codom = a1.Codom} =
    new Nat[c1,c2]:
      self=>
      type F1[t] = a2.F1[t]
      type F2[t] = a1.F2[t]
      def apply[T]: c2{type Dom = self.Dom#F[T]; type Codom = self.Codom#F[T] } =
        val t2: Arrow.Aux[c2,a2.Dom#F[T], a2.Codom#F[T]] = a2[T]
        val t1: Arrow.Aux[c2,a1.Dom#F[T], a1.Codom#F[T]] = a1[T]
        val postulateToWorkAroundTypeSystemLimitation: a2.Codom#F[T] =:= a1.Dom#F[T] = proof.asInstanceOf
        val tst = postulateToWorkAroundTypeSystemLimitation.liftCo[[q]=>>Arrow.Aux[c2,a2.Dom#F[T], q]]
        val t3: Arrow.Aux[c2, a2.F1[T], a1.F1[T]] = tst(t2)
        val t4  = c2Cat.compose(t1,t3)
        t4
