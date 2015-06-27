package chapter4

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  //  def orElse[B >: A](ob: => Option[B]): Option[B]
  //  def filter(f: A=> Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = default
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
}


object Vaian {
  def mean(xs: Seq[Double]): scala.Option[Double] = {
    if (xs.isEmpty) scala.None
    else scala.Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): scala.Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}



