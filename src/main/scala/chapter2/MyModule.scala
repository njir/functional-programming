package chapter2

/**
 * Created by tak on 15. 5. 24.
 */
object MyModule {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int) : Int = {
      val next = n + acc
      if()
      else go(acc, next)
    }
  }
}