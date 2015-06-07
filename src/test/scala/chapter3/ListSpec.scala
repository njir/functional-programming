package chapter3

import org.scalatest.{Matchers, FunSuite}

class ListSpec extends FunSuite with Matchers {
  test("List sum") {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)

    List.sum(a) should equal(6)
    List.sum(b) should equal(15)
    List.sum2(a) should equal(List.sum(a))
    List.sum2(b) should equal(15)
  }

  test("List product") {
    val c = List(1.3, 2.3, 0.0)
    val d = List(1.0, 2.0, 3.0)

    List.product(c) should equal(0.0)
    List.product(d) should equal(6.0)
    List.product2(d) should equal(List.product(d))
  }

  test("List append, drop") {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)
    val c = List(1, 2, 3, 4, 5)

    List.append(a, b) should equal(List(1, 2, 3, 4, 5, 6))
    List.dropWhile(c)(x => x < 4) should equal(List(4, 5))
  }

  test("List drop, tail, init"){
    val a = List(1,2,3)
    val b = List(5,4,3,2,1)
    val c = List(1)

    List.tail(a) should equal (List(2,3))
    List.tail(c) should equal (Nil)

    List.drop(a)(2) should equal (List(3))
    List.drop(a)(5) should equal (Nil)
    List.drop(c)(3) should equal (Nil)


  }

}
