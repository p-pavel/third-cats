package com.perikov.thirdcats.rec_schemes

object FuncExtensions:
  extension[A, B, C] (f: A => B) def >>>(g: B => C): A => C = f andThen g
  extension[A, B, C] (f: B => C) def <<<(g: A => B): A => C = g andThen f
  extension[A, B, C] (f: A => B) def &&&(g: A => C): A => (B, C) = a => (f(a), g(a))
