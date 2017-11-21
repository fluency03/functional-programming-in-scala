package com.fluency03.fpscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def maximum[A](t: Tree[A])(f: (A, A) => A): A = t match {
    case Leaf(v) => v
    case Branch(l, r) => f(maximum(l)(f), maximum(r)(f))
  }

  def fold[IN, ACC](t: Tree[IN])(f: IN => ACC)(g: (ACC, ACC) => ACC): ACC = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // TODO (Chang): fold with initial value?
  def foldWithInit[IN, ACC](t: Tree[IN], z: ACC)(f: (IN, ACC) => ACC)(g: (ACC, ACC) => ACC): ACC = t match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => g(foldWithInit(l, z)(f)(g), foldWithInit(r, z)(f)(g))
  }

  def sizeByFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumByFold(t: Tree[Int]): Int =
    fold(t)(a  => a)(_ max _)

  def depthByFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  /*
  type mismatch

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
  annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
  to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
  common to define helper functions that simply call the corresponding data constructors but give the less specific
  result type:
  */
//  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
//    fold(t)(a => Leaf(f(a)))(Branch(_,_))

}
