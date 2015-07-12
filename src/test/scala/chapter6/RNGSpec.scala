package chapter6

import org.scalatest.{Matchers, FunSuite}

/**
 * Created by tak on 15. 7. 12.
 */
class RNGSpec extends FunSuite with Matchers {
  test("RNG"){
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n3, rng3) = rng.nonNegativeInt(rng)
    val (n4, rng4) = rng.double(rng)

    (n3 >= 0) shouldBe true

    println (n4)


  }

}
