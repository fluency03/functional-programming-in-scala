package com.fluency03.fpscala.datastructures

import org.scalatest._

class TreeTest extends FlatSpec with Matchers {

  "size" should "return a the size of a Tree." in {
    val l1 = Leaf(1)
    Tree.size(l1) should equal (1)

    val l2 = Leaf(2)
    val b1 = Branch(l1, l2)
    Tree.size(b1) should equal (3)

    val l3 = Leaf(3)
    val b2 = Branch(b1, l3)
    Tree.size(b2) should equal (5)
  }

  "maximum" should "return the maximum element of a Tree." in {
    val l1 = Leaf(1)
    Tree.maximum(l1) should equal (1)

    val l2 = Leaf(2)
    val b1 = Branch(l1, l2)
    Tree.maximum(b1) should equal (2)

    val l3 = Leaf(3)
    val b2 = Branch(b1, l3)
    Tree.maximum(b2) should equal (3)
  }

  "depth" should "return the depth of a Tree." in {
    val l1 = Leaf(1)
    Tree.depth(l1) should equal (0)

    val l2 = Leaf(2)
    val b1 = Branch(l1, l2)
    Tree.depth(b1) should equal (1)

    val l3 = Leaf(3)
    val b2 = Branch(b1, l3)
    Tree.depth(b2) should equal (2)

    val l4 = Leaf(4)
    val b3 = Branch(b2, l4)
    Tree.depth(b3) should equal (3)
  }

  "map" should "return a new Tree by applying the given function on a Tree." in {
    val intToString = (i: Int) => i.toString

    Tree.map(Leaf(1))(intToString) should equal (Leaf("1"))
    Tree.map(
      Branch(Leaf(1), Leaf(2))
    )(intToString) should equal (
      Branch(Leaf("1"), Leaf("2"))
    )
    Tree.map(
      Branch(Branch(Leaf(1), Leaf(2)), Leaf(2))
    )(intToString) should equal (
      Branch(Branch(Leaf("1"), Leaf("2")), Leaf("2"))
    )
  }

  "maximum" should "return the maximum element of a Tree, given a customized compare function." in {
    val comp = (a: Double, b: Double) => a max b

    val l1 = Leaf(1.0)
    Tree.maximum(l1)(comp) should equal (1.0)

    val l2 = Leaf(2.0)
    val b1 = Branch(l1, l2)
    Tree.maximum(b1)(comp) should equal (2.0)

    val l3 = Leaf(3.0)
    val b2 = Branch(b1, l3)
    Tree.maximum(b2)(comp) should equal (3.0)
  }

  "fold" should "accumulate a Tree to a value." in {
    pending
  }

  "foldWithInit" should "accumulate a Tree to a value from a initial value." in {
    pending
  }

  "sizeByFold" should "return a the size of a Tree." in {
    pending
  }

  "maximumByFold" should "return the maximum element of a Tree." in {
    pending
  }

  "depthByFold" should "return the depth of a Tree." in {
    pending
  }

  "mapByFold" should "return a new Tree by applying the given function on a Tree." in {
    pending
  }

}
