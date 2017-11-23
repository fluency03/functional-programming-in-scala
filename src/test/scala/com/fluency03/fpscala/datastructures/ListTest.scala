package com.fluency03.fpscala.datastructures

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "A Nil" should "represent an empty List." in {
    Nil should equal(List[Nothing]())
  }

  "A Cons" should "represent a List with its head and tail." in {
    val c = Cons(1, Nil)
    c shouldBe a [List[_]]
    c should equal(List(1))
    c.head should equal(1)
    c.tail should equal(Nil)
  }

  "apply" should "construct a List." in {
    List() should equal(Nil)
    List(1) should equal(Cons(1, Nil))
    List(1, 2) should equal(Cons(1, Cons(2, Nil)))
    List(1, 2, 3) should equal(Cons(1, List(2, 3)))
  }

  "sum" should "sum the ints from a List." in {
    val l0 = Nil
    List.sum(l0) should equal(0)

    val l1 = List(5)
    List.sum(l1) should equal(5)

    val l2 = List(1, 2, 3, 4, 5)
    List.sum(l2) should equal(15)
  }

  "product" should "multiply the ints from a List." in {
    val l0 = Nil
    List.product(l0) should equal(1.0)

    val l1 = List(5.0)
    List.product(l1) should equal(5.0)

    val l2 = List(5.0, 0.0)
    List.product(l2) should equal(0.0)

    val l3 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    List.product(l3) should equal(120.0)
  }

  "tail" should "return the tail List of input List." in {
    the [NoSuchElementException] thrownBy {
      List.tail(Nil)
    } should have message "Cannot get tail from an empty list."

    List.tail(List(1)) should equal(Nil)
    List.tail(List(1, 2)) should equal(List(2))
    List.tail(List(1, 2, 3)) should equal(List(2, 3))
  }

  "setHead" should "set a new head of the given List." in {
    the [NoSuchElementException] thrownBy {
      List.setHead(Nil, 1)
    } should have message "Cannot set head for an empty list."

    List.setHead(List(1), 2) should equal(List(2))
    List.setHead(List(1, 2), 2) should equal(List(2, 2))
    List.setHead(List(1, 2, 3), 2) should equal(List(2, 2, 3))
  }

  "drop" should "drop the first n elements from a List." in {
    List.drop(List(1), -1) should equal(List(1))
    List.drop(List(1), 0) should equal(List(1))

    List.drop(List(1, 2, 3), 1) should equal(List(2, 3))
    List.drop(List(1, 2, 3), 2) should equal(List(3))
    List.drop(List(1, 2, 3), 3) should equal(Nil)
  }

  "dropWhile" should "drop all element which are making the condition true from the begining of a List." in {
    List.dropWhile[Int](Nil, _ > 0) should equal(Nil)
    List.dropWhile[Int](List(1, -1), _ > 0) should equal(List(-1))
    List.dropWhile[Int](List(1), _ > 0) should equal(Nil)
    List.dropWhile[Int](List(-1), _ > 0) should equal(List(-1))
    List.dropWhile[Int](List(-1, 2, 3), _ > 0) should equal(List(-1, 2, 3))
    List.dropWhile[Int](List(1, -2, -3), _ > 0) should equal(List(-2, -3))
    List.dropWhile[Int](List(-1, 2, -3), _ > 0) should equal(List(-1, 2, -3))
    List.dropWhile[Int](List(1, -2, 3), _ > 0) should equal(List(-2, 3))
    List.dropWhile[Int](List(1, 2, 3), _ > 0) should equal(Nil)
  }

  "dropWhile2" should "drop all element which are making the condition true from the beginning of a List." in {
    List.dropWhile2[Int](Nil)(_ > 0) should equal(Nil)
    List.dropWhile2[Int](List(1, -1))(_ > 0) should equal(List(-1))
    List.dropWhile2[Int](List(1))(_ > 0) should equal(Nil)
    List.dropWhile2[Int](List(-1))(_ > 0) should equal(List(-1))
    List.dropWhile2[Int](List(-1, 2, 3))(_ > 0) should equal(List(-1, 2, 3))
    List.dropWhile2[Int](List(1, -2, -3))(_ > 0) should equal(List(-2, -3))
    List.dropWhile2[Int](List(-1, 2, -3))(_ > 0) should equal(List(-1, 2, -3))
    List.dropWhile2[Int](List(1, -2, 3))(_ > 0) should equal(List(-2, 3))
    List.dropWhile2[Int](List(1, 2, 3))(_ > 0) should equal(Nil)
  }

  "append" should "append one List at the end of another." in {
    List.append(Nil, Nil) should equal(Nil)
    List.append(Nil, List(1)) should equal(List(1))
    List.append(List(1), Nil) should equal(List(1))
    List.append(List(1, 2), List(3, 4)) should equal(List(1, 2, 3, 4))
  }

  "init" should "returns a List consisting of all but the last element of a List." in {
    the [NoSuchElementException] thrownBy {
      List.init(Nil)
    } should have message "No initial element for an empty list."

    List.init(List(1)) should equal(Nil)
    List.init(List(1, 2)) should equal(List(1))
    List.init(List(1, 2, 3)) should equal(List(1, 2))
  }

  "initTailRec" should "returns a List consisting of all but the last element of a List." in {
    the [NoSuchElementException] thrownBy {
      List.initTailRec(Nil)
    } should have message "No initial element for an empty list."

    List.initTailRec(List(1)) should equal(Nil)
    List.initTailRec(List(1, 2)) should equal(List(1))
    List.initTailRec(List(1, 2, 3)) should equal(List(1, 2))
  }

  "foldRight" should "accumulate the List from Right by applying given function." in {
    val l = List(1, 2, 3, 4, 5)
    List.foldRight(l, 1)(_ + _) should equal(16)

    val strs = List("a", "b", "c", "d", "e")
    List.foldRight(strs, "0")(_ + "<-" + _) should equal("a<-b<-c<-d<-e<-0")
  }

  "sum2" should "sum the ints from a List." in {
    val l0 = Nil
    List.sum2(l0) should equal(0)

    val l1 = List(5)
    List.sum2(l1) should equal(5)

    val l2 = List(1, 2, 3, 4, 5)
    List.sum2(l2) should equal(15)
  }

  "product2" should "multiply the ints from a List." in {
    val l0 = Nil
    List.product2(l0) should equal(1.0)

    val l1 = List(5.0)
    List.product2(l1) should equal(5.0)

    val l2 = List(5.0, 0.0)
    List.product2(l2) should equal(0.0)

    val l3 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    List.product2(l3) should equal(120.0)
  }

  "length" should "return the length of a List." in {
    List.length(Nil) should equal(0)
    List.length(List()) should equal(0)
    List.length(List(1)) should equal(1)
    List.length(List("a", "b", "c")) should equal(3)
  }

  "foldLeft" should "accumulate the List from Right by applying given function." in {
    val l = List(1, 2, 3, 4, 5)
    List.foldLeft(l, 1)(_ + _) should equal(16)

    val strs = List("a", "b", "c", "d", "e")
    List.foldLeft(strs, "0")(_ + "->" + _) should equal("0->a->b->c->d->e")
  }

  "sum3" should "sum the ints from a List." in {
    val l0 = Nil
    List.sum3(l0) should equal(0)

    val l1 = List(5)
    List.sum3(l1) should equal(5)

    val l2 = List(1, 2, 3, 4, 5)
    List.sum3(l2) should equal(15)
  }

  "product3" should "multiply the ints from a List." in {
    val l0 = Nil
    List.product3(l0) should equal(1.0)

    val l1 = List(5.0)
    List.product3(l1) should equal(5.0)

    val l2 = List(5.0, 0.0)
    List.product3(l2) should equal(0.0)

    val l3 = List(1.0, 2.0, 3.0, 4.0, 5.0)
    List.product3(l3) should equal(120.0)
  }

  "length2" should "return the length of a List." in {
    List.length2(Nil) should equal(0)
    List.length2(List()) should equal(0)
    List.length2(List(1)) should equal(1)
    List.length2(List("a", "b", "c")) should equal(3)
  }

  "reverse" should "return a new List with revered order." in {
    List.reverse(Nil) should equal(Nil)
    List.reverse(List()) should equal(List())
    List.reverse(List(1)) should equal(List(1))
    List.reverse(List("a", "b", "c")) should equal(List("c", "b", "a"))
  }

  "foldRightByFoldLeft" should "accumulate the List from Right by applying given function." in {
    val l = List(1, 2, 3, 4, 5)
    List.foldRightByFoldLeft(l, 1)(_ + _) should equal(16)

    val strs = List("a", "b", "c", "d", "e")
    List.foldRightByFoldLeft(strs, "0")(_ + "<-" + _) should equal("a<-b<-c<-d<-e<-0")
  }

  "foldLeftByFoldRight" should "accumulate the List from Left by applying given function." in {
    val l = List(1, 2, 3, 4, 5)
    List.foldLeftByFoldRight(l, 1)(_ + _) should equal(16)

    val strs = List("a", "b", "c", "d", "e")
    List.foldLeftByFoldRight(strs, "0")(_ + "->" + _) should equal("0->a->b->c->d->e")
  }

  "appendByFoldRight" should "return a new List one List appended after another." in {
    List.appendByFoldRight(Nil, Nil) should equal(Nil)
    List.appendByFoldRight(Nil, List(1)) should equal(List(1))
    List.appendByFoldRight(List(1), Nil) should equal(List(1))
    List.appendByFoldRight(List(1, 2), List(3, 4)) should equal(List(1, 2, 3, 4))
  }

  "concat" should "return a new List with all given Lists concatenated together." in {
    val lOfL = List(
      List(1, 2, 3), Nil, List(4, 5, 6), List(), List(7, 8)
    )
    List.concat(lOfL) should equal(List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  "addOne" should "return a new List with all elements added by 1 from given List." in {
    List.addOne(List()) should equal(Nil)
    List.addOne(Nil) should equal(Nil)
    List.addOne(List(1, 2, 3)) should equal(List(2, 3, 4))
  }

  "doubleToString" should "return a new List with all String elements converted from given Double List." in {
    List.doubleToString(List()) should equal(Nil)
    List.doubleToString(Nil) should equal(Nil)
    List.doubleToString(List(1.0, 2.0, 3.0)) should equal(List("1.0", "2.0", "3.0"))
  }

  "map" should "return a new List by applying given function on each element of input List." in {
    val addOne = (i: Int) => i + 1
    val doubleToString = (d: Double) => d.toString

    List.map(List())(addOne) should equal(Nil)
    List.map(Nil)(addOne) should equal(Nil)
    List.map(List(1, 2, 3))(addOne) should equal(List(2, 3, 4))

    List.map(List())(doubleToString) should equal(Nil)
    List.map(Nil)(doubleToString) should equal(Nil)
    List.map(List(1.0, 2.0, 3.0))(doubleToString) should equal(List("1.0", "2.0", "3.0"))
  }

  "map2" should "return a new List by applying given function on each element of input List." in {
    val addOne = (i: Int) => i + 1
    val doubleToString = (d: Double) => d.toString

    List.map2(List())(addOne) should equal(Nil)
    List.map2(Nil)(addOne) should equal(Nil)
    List.map2(List(1, 2, 3))(addOne) should equal(List(2, 3, 4))

    List.map2(List())(doubleToString) should equal(Nil)
    List.map2(Nil)(doubleToString) should equal(Nil)
    List.map2(List(1.0, 2.0, 3.0))(doubleToString) should equal(List("1.0", "2.0", "3.0"))
  }

  "map3" should "return a new List by applying given function on each element of input List." in {
    val addOne = (i: Int) => i + 1
    val doubleToString = (d: Double) => d.toString

    List.map2(List())(addOne) should equal(Nil)
    List.map2(Nil)(addOne) should equal(Nil)
    List.map2(List(1, 2, 3))(addOne) should equal(List(2, 3, 4))

    List.map2(List())(doubleToString) should equal(Nil)
    List.map2(Nil)(doubleToString) should equal(Nil)
    List.map2(List(1.0, 2.0, 3.0))(doubleToString) should equal(List("1.0", "2.0", "3.0"))
  }

  "filter" should "return a new List with elements from given List meeting given condition. " in {
    val isEven = (i: Int) => i%2 == 0
    val isLargerThanFive = (d: Double) => d > 5.0

    List.filter(List())(isEven) should equal(Nil)
    List.filter(Nil)(isEven) should equal(Nil)
    List.filter(List(1, 2, 3, 4, 5))(isEven) should equal(List(2, 4))

    List.filter(List())(isLargerThanFive) should equal(Nil)
    List.filter(Nil)(isLargerThanFive) should equal(Nil)
    List.filter(List(2.0, 3.0, 6.0 , 5.0))(isLargerThanFive) should equal(List(6.0))
  }

  "filter2" should "return a new List with elements from given List meeting given condition. " in {
    val isEven = (i: Int) => i%2 == 0
    val isLargerThanFive = (d: Double) => d > 5.0

    List.filter2(List())(isEven) should equal(Nil)
    List.filter2(Nil)(isEven) should equal(Nil)
    List.filter2(List(1, 2, 3, 4, 5))(isEven) should equal(List(2, 4))

    List.filter2(List())(isLargerThanFive) should equal(Nil)
    List.filter2(Nil)(isLargerThanFive) should equal(Nil)
    List.filter2(List(2.0, 3.0, 6.0 , 5.0))(isLargerThanFive) should equal(List(6.0))
  }

  "filter3" should "return a new List with elements from given List meeting given condition. " in {
    val isEven = (i: Int) => i%2 == 0
    val isLargerThanFive = (d: Double) => d > 5.0

    List.filter3(List())(isEven) should equal(Nil)
    List.filter3(Nil)(isEven) should equal(Nil)
    List.filter3(List(1, 2, 3, 4, 5))(isEven) should equal(List(2, 4))

    List.filter3(List())(isLargerThanFive) should equal(Nil)
    List.filter3(Nil)(isLargerThanFive) should equal(Nil)
    List.filter3(List(2.0, 3.0, 6.0 , 5.0))(isLargerThanFive) should equal(List(6.0))
  }

  "flatMap" should "return a new List by concatenating all Lists generated by splitting each element of input List." in {
    val splitBySpace = (s: String) => List(s.split(" "):_*)

    List.flatMap(List())(splitBySpace) should equal(Nil)
    List.flatMap(Nil)(splitBySpace) should equal(Nil)
    List.flatMap(List("this is a message", ": hello world!"))(splitBySpace) should equal(
      List("this", "is", "a", "message", ":", "hello", "world!")
    )
  }

  "filterByFlatMap" should "return a new List with elements from given List meeting given condition. " in {
    val isEven = (i: Int) => i%2 == 0
    val isLargerThanFive = (d: Double) => d > 5.0

    List.filterByFlatMap(List())(isEven) should equal(Nil)
    List.filterByFlatMap(Nil)(isEven) should equal(Nil)
    List.filterByFlatMap(List(1, 2, 3, 4, 5))(isEven) should equal(List(2, 4))

    List.filterByFlatMap(List())(isLargerThanFive) should equal(Nil)
    List.filterByFlatMap(Nil)(isLargerThanFive) should equal(Nil)
    List.filterByFlatMap(List(2.0, 3.0, 6.0 , 5.0))(isLargerThanFive) should equal(List(6.0))
  }

  "addPairwise" should "add the elements from two Lists pairwise." in {
    List.addPairwise(List(), Nil) should equal(Nil)
    List.addPairwise(List(1), Nil) should equal(Nil)
    List.addPairwise(Nil, List(1)) should equal(Nil)
    List.addPairwise(List(1, 2), List(3, 4)) should equal(List(4, 6))
  }

  "zipWith" should "apply the given function by taking the elements from two Lists pairwise." in {
    val addInt = (a: Int, b: Int) => a + b
    val addStr = (a: String, b: String) => a + b

    List.zipWith(List(), Nil)(addInt) should equal(Nil)
    List.zipWith(List(1), Nil)(addInt) should equal(Nil)
    List.zipWith(Nil, List(1))(addInt) should equal(Nil)
    List.zipWith(List(1, 2), List(3, 4))(addInt) should equal(List(4, 6))

    List.zipWith(List(), Nil)(addStr) should equal(Nil)
    List.zipWith(List("1"), Nil)(addStr) should equal(Nil)
    List.zipWith(Nil, List("1"))(addStr) should equal(Nil)
    List.zipWith(List("a", "b"), List("1", "2"))(addStr) should equal(List("a1", "b2"))
  }

  "startsWith" should "check whether one List is the first part of another." in {
    List.startsWith(List(), Nil) should equal(true)
    List.startsWith(List(1), Nil) should equal(true)
    List.startsWith(Nil, List(1)) should equal(false)
    List.startsWith(List(1, 2), List(3, 4)) should equal(false)
    List.startsWith(List(1, 2), List(1, 2)) should equal(true)
    List.startsWith(List(1, 2), List(1)) should equal(true)
    List.startsWith(List(1, 2), List(2)) should equal(false)
  }

  "hasSubsequence" should "check whether one List is a sub-sequence of another." in {
    List.hasSubsequence(List(), Nil) should equal(true)
    List.hasSubsequence(List(1), Nil) should equal(true)
    List.hasSubsequence(Nil, List(1)) should equal(false)
    List.hasSubsequence(List(1, 2), List(3, 4)) should equal(false)
    List.hasSubsequence(List(1, 2), List(1, 2)) should equal(true)
    List.hasSubsequence(List(1, 2), List(1)) should equal(true)
    List.hasSubsequence(List(1, 2), List(2)) should equal(true)
  }

}
