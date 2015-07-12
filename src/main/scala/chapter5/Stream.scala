package chapter5

import scala.collection.immutable.Stream.cons

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists2(p)
    case _ => false
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def forAll2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll2(p)
    case _ => true
  }

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

  def map2[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => Stream.cons(f(h()), t().map2(f))
    case _ => Empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if(n > 1) => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if(n == 1) => Stream.cons(h(), Empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if(n >= 1) => t().drop(n - 1)
    case _ => this
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] =
    Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A) : Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def from2(n: Int) = unfold(n)(n => Some(n, n+1))
  def constant2[A](a: A) = unfold(a)(_ => Some(a, a))

}

