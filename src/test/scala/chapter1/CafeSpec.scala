package chapter1

import org.scalatest.{FunSuite, Matchers}

/**
 * Created by tak on 15. 5. 24.
 */

class CafeSpec extends FunSuite with Matchers {
  test("buy coffee") {
    val cafe = new Cafe3
    val card = new CreditCard

    val (coffees, charge) = cafe.buyCoffees(card, 10)
    assert(coffees.length == 10)
    assert(charge.amount == 300)
  }
}
