object CourseFunctions extends App {
	def IF[T](expr: Boolean, success: T, fail: T): T = if (expr) success else fail

	def rev[U](hand: List[U], table: List[U]): List[U] = hand match {
		case Nil => table
		case a :: tail => rev(tail, a :: table)
	}

	def reverse[U](list: List[U]): List[U] = { rev(list, List()) }

	def take(n: Int, L: List[Any]): List[Any] = (n, L) match {
		case (_, Nil) => Nil
		case (0, _) => Nil
		case (x, a::tail) => a::take(x-1, tail)
	}
	
	def skip(n: Int, L: List[Any]): List[Any] = (n, L) match {
		case (_, Nil) => Nil
		case (0, L) => L
		case (x, a::tail) => skip(x-1, tail)
	}

	def every(n: Int, L: List[Any]): List[Any] = (n, L) match {
		case (_, Nil) => Nil
		case (0, _) => Nil
		case (x, a::tail) => a::every(x, skip(x-1, tail))
	}

	def slice(m: Int, n: Int, L: List[Any]): List[Any] = {
		take(n - m, skip(m, L))
	}

	assert((take(3, List(9, 2, 3, 5, 2, 74, 12))) == List(9, 2, 3))
	assert((skip(3, List(9, 2, 3, 5, 2, 74, 12))) == List(5, 2, 74, 12))
	assert((slice(3, 5, List(9, 2, 3, 5, 2, 74, 12))) == List(5, 2))
	assert((every(3, List(9, 2, 3, 5, 2, 74, 12))) == List(9, 5, 12))

	def foreach[U, V](L: List[U], f: (V, U) => V, s: V): V = (L, s) match {
		case (Nil, result) => result
		case (a::tail, result) => foreach(tail, f, f(result, a))
	}

	val sum = (L: List[Int]) => (foreach(L, (s: Int, x: Int) => s + x, 0)): Int
	val count = (a: Int, L: List[Int]) => (foreach(L, (s: Int, x: Int) => IF(a == x, s + 1, s), 0)): Int
	val map = (f: (Int => Int), L: List[Int]) => (foreach(reverse(L), (S: List[Int], x: Int) => (f(x)::S: List[Int]), Nil)): List[Int]
	val filter = (f: (Int => Boolean), L: List[Int]) => (foreach(reverse(L), (S: List[Int], x: Int) => IF(f(x), x::S, S): List[Int], Nil)): List[Int]
	val reduce = (f: (Int, Int) => Int, L: List[Int]) => (L match { case Nil => 0; case x::tail => foreach(tail, f, x) }): Int

	assert(sum(List(1, 2, 3, 4)) == 10)
	assert(count(1, List(1, 2, 3, 4, 1)) == 2)
	assert(map((a: Int) => (a*a: Int), List(1, 2, 3, 4)) == List(1, 4, 9, 16))
	assert(filter((a: Int) => (a >= 3: Boolean), List(1, 2, 3, 4)) == List(3, 4))
	assert(reduce((a: Int, b: Int) => (a * b): Int, List(1, 2, 3, 4)) == 24)
}
