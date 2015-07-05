package chapter5

import org.scalatest.{FunSuite, Matchers}
import Stream._

class StreamSpec extends FunSuite with Matchers{
  test("Stream"){
    val s = cons(1, cons(2, cons(3, Empty)))

    s.forAll(_ < 4) should equal (true)
    s.forAll(_ < 2) should equal (false)
  }

  test("map"){
    val s = cons(1, cons(2, Empty))
    val s2 = cons(3, cons(4, Empty))

    s.map(_ + 2).forAll(_ >= 3) should be (true)
  }

}
