package com.fluency03.fpscala.laziness

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    // Explicit forcing of the h thunk using h()
    // Evaluate only the portion actually demanded (we don’t evaluate the tail of the Cons)
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(x, xs) => go(xs(), x() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  @annotation.tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsByFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapByUnfold[B](f: A => B): Stream[B] =
    unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def takeByUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), ni) if ni > 1 => Some((h(), (t(), ni - 1)))
      case _ => None
    }

  def takeWhileByUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) => Some(((Option.empty[A], Some(h())), (empty[A], t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipWith(s)((a, b) => a == b).forAll(bo => bo)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

//  def tails: Stream[Stream[A]] =
//    unfold(this) {
//      case Cons(h, t) => Some((this, t()))
//      case _ => None
//    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2


}

case object Empty extends Stream[Nothing]

/**
 * A nonempty stream consists of a head and a tail, which are both non-strict.
 * Due to technical limitations, these are thunks that must be explicitly forced,
 * rather than by-name parameters.
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  /**
   * Smart constructor.
   *
   * It takes care of memoizing the by-name arguments for the head and
   * tail of the Cons. This is a common trick, and it ensures that our thunk will only do
   * its work once, when forced for the first time.
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // We cache the head and tail as lazy values to avoid repeated evaluation.
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constantLessEfficient[A](a: A): Stream[A] =
    cons(a, constantLessEfficient(a))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def loop(i1: Int, i2: Int): Stream[Int] = {
      cons(i1, loop(i2, i1 + i2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def unfoldByFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty: Stream[A])((p: (A, S)) => cons(p._1, unfoldByFold(p._2)(f)))

  def unfoldByMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfoldByMap(p._2)(f))).getOrElse(empty)

  def fibsByUnfold(): Stream[Int] =
    unfold((0, 1)) {
      case (i1, i2) => Some(i2, (i2, i1 + i2))
    }

  def fromByUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantByUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def onesByUnfold(): Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

//  def onesByUnfold(): Stream[Int] = constantByUnfold(1)

}
