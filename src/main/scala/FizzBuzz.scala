import java.io.PrintStream

def printTiming(action: => Unit) =
  val start = System.nanoTime()
  action
  val stop = System.nanoTime()
  println(s"Took ${(stop - start)/1e9} seconds")

def toName(n: Int): String =
  def divisible(k: Int) = n % k == 0
  (divisible(3), divisible(5)) match
    case (true, true) => "FizzBuzz"
    case (true, _)    => "Fizz"
    case (_, true)    => "Buzz"
    case _            => n.toString


@main
def testFizzBuzz() =
  val limit = 10_000_000
  val out = new PrintStream("/dev/console")
  printTiming(1 to limit foreach (toName andThen out.print))