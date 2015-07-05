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

}

