package chapter4

import org.scalatest.{Matchers, FunSuite}

class OptionSpec extends FunSuite with Matchers {
  test("Option") {
    val a = Some(1)
    val b = Some(4)

    a.map(_ + 3) should equal (b)
    b.map(_ - 3) should equal (a)

    val map = Map("Hi" -> "Dan", "Hello" -> "dddddd")
    map.getOrElse("Hi", "No key with that name!") should equal ("Dan")
    map.getOrElse("Hello", "No key with that name!") should equal ("dddddd")
    map.getOrElse("Hello2", "No key with that name!") should equal ("No key with that name!")

  }
}