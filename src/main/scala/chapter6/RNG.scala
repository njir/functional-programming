package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
   rng => {
     val (a, rngA) = f(rng)
     g(a)(rngA) // RAND[B] = rng => (B, rng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    (if(n < 0) -n else n, rng1)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double2(rng: RNG) : (Double, RNG) = {
    val (n, rng1) = nonNegativeInt(rng)

    if(n == Int.MaxValue) ((n - 1).toDouble / Int.MaxValue, rng1)
    else (n.toDouble / Int.MaxValue, rng1)
  }

  def double: Rand[Double] = map(nonNegativeInt) { n =>
    if (n == Int.MaxValue) (n - 1).toDouble / Int.MaxValue
    else n.toDouble / Int.MaxValue
  }
}
