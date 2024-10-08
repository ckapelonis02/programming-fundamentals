object CourseFunctions extends App {
  
  def IF[T](expr: Boolean, success: T, fail: T): T = if (expr) success else fail

  assert(IF(5 == 3, "success", "fail") == "fail")
  assert(IF("scala" == "scala", "success", "fail") == "success")

  def rev[U](hand: List[U], table: List[U]): List[U] = hand match {
    case Nil         => table
    case a :: tail   => rev(tail, a :: table)
  }

  def reverse[U](list: List[U]): List[U] = rev(list, List())

  assert(reverse(List(9, 2, 3)) == List(3, 2, 9))

  def take[U](n: Int, L: List[U]): List[U] = (n, L) match {
    case (_, Nil)       => Nil
    case (0, _)         => Nil
    case (x, a :: tail) => a :: take(x - 1, tail)
  }

  def skip[U](n: Int, L: List[U]): List[U] = (n, L) match {
    case (_, Nil)       => Nil
    case (0, L)         => L
    case (x, a :: tail) => skip(x - 1, tail)
  }

  def every[U](n: Int, L: List[U]): List[U] = (n, L) match {
    case (_, Nil)       => Nil
    case (0, _)         => Nil
    case (x, a :: tail) => a :: every(x, skip(x - 1, tail))
  }

  def slice[U](m: Int, n: Int, L: List[U]): List[U] = take(n - m, skip(m, L))

  assert(take(3, List(9, 2, 3, 5, 2, 74, 12)) == List(9, 2, 3))
  assert(skip(3, List(9, 2, 3, 5, 2, 74, 12)) == List(5, 2, 74, 12))
  assert(slice(3, 5, List(9, 2, 3, 5, 2, 74, 12)) == List(5, 2))
  assert(every(3, List(9, 2, 3, 5, 2, 74, 12)) == List(9, 5, 12))

  def foreach[U, V](L: List[U], f: (V, U) => V, s: V): V = (L, s) match {
    case (Nil, result)       => result
    case (a :: tail, result) => foreach(tail, f, f(result, a))
  }

  def sum[U](L: List[U])(implicit num: Numeric[U]): U = 
    foreach(L, (s: U, x: U) => num.plus(s, x), num.zero)

  def count[U](a: U, L: List[U])(implicit num: Numeric[U]): U = 
    foreach(L, (s: U, x: U) => IF(a == x, num.plus(s, num.one), s), num.zero)

  def map[U](f: U => U, L: List[U]): List[U] = 
    foreach(reverse(L), (S: List[U], x: U) => f(x) :: S, Nil)

  def filter[U](f: U => Boolean, L: List[U]): List[U] = 
    foreach(reverse(L), (S: List[U], x: U) => IF(f(x), x :: S, S), Nil)

  def reduce[U](f: (U, U) => U, L: List[U])(implicit num: Numeric[U]): U = 
    L match {
      case Nil        => num.zero
      case x :: tail  => foreach(tail, f, x)
    }

  assert(sum(List(1, 2, 3, 4)) == 10)
  assert(count(1, List(1, 2, 3, 4, 1)) == 2)
  assert(map((a: Int) => a * a, List(1, 2, 3, 4)) == List(1, 4, 9, 16))
  assert(filter((a: Int) => a >= 3, List(1, 2, 3, 4)) == List(3, 4))
  assert(reduce((a: Int, b: Int) => a * b, List(1, 2, 3, 4)) == 24)
}
