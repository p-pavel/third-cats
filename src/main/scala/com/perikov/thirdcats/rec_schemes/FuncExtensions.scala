package com.perikov.thirdcats.rec_schemes

object FuncExtensions:
  extension[A, B, C] (f: A => B) def >>>(g: B => C): A => C = f andThen g
  extension[A, B, C] (f: B => C) def <<<(g: A => B): A => C = g andThen f
  extension[A, B, C] (f: A => B) def &&&(g: A => C): A => (B, C) = a => (f(a), g(a))
  extension[A, B, C] (f: A => C) def |||(g: B => C): Either[A,B] => C =
    case Left(t) => f(t)
    case Right(t)=> g(t)
  extension [A,B   ] (f: A => B) def ***[C,D](g: C => D): (A,C) => (B,D) = (a,c) => (f(a),g(c))
