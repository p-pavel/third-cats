def thrice[A](f: A => A): A => A = f andThen f andThen f

extension [A,B](f: A => B) def $(arg: A): B = f(arg)
thrice(thrice(thrice[Int](1+_)))(0)