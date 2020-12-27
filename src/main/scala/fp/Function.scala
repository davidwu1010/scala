package fp

object Function extends App {
  val concat = (v1: String, v2: String) => v1 + v2
  val foo: (Int) => ((Int) => Int) = (x: Int) => (y: Int) => x + y

  def toCurry(f: (Int, Int) => Int): Int => Int => Int = (x: Int) => (y: Int) => f(x, y)
  def fromCurry(f: Int => Int => Int): (Int, Int) => Int = (x, y) => f(x)(y)

  def compose[A, B, T](f: A => B, g: T => A): T => B = x => f(g(x))
  def andThen[A, B, C](f: A => B, g: B => C): A => C = x => g(f(x))
}
