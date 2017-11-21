package com.fluency03.fpscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val a: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => throw new NoSuchElementException("Cannot get tail from an empty list.")
    case Cons(_, xs) => xs
  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => throw new NoSuchElementException("Cannot set head for an empty list.")
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if ( n <= 0 ) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("No initial element for an empty list.")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def initTailRec[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def loop(cur: List[A]): List[A] = cur match {
      case Nil => throw new NoSuchElementException("No initial element for an empty list.")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; loop(t)
    }
    loop(l)
  }

  // not stack-safes
  def foldRight[IN, ACC](as: List[IN], z: ACC)(f: (IN, ACC) => ACC): ACC = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  @annotation.tailrec
  def foldLeft[IN, ACC](as: List[IN], z: ACC)(f: (ACC, IN) => ACC): ACC =  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))

  // stack-safe
  def foldRightByFoldLeft[IN, ACC](l: List[IN], z: ACC)(f: (IN, ACC) => ACC): ACC =
    foldLeft(l, (b: ACC) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftByFoldRight[IN, ACC](l: List[IN], z: ACC)(f: (ACC, IN) => ACC): ACC =
    foldRight(l, (b: ACC) => b)((a, g) => b => g(f(b, a)))(z)

  def appendByFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(appendByFoldRight)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def map2[A, B](l: List[A])(f: A => B): List[B] =
    foldRightByFoldLeft(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def map3[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    @annotation.tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); loop(t)
    }
    loop(l)
    List(buf.toList: _*)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightByFoldLeft(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def filter3[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; loop(t)
    }
    loop(l)
    List(buf.toList: _*)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

}
