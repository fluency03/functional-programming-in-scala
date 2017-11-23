package com.fluency03.fpscala.datastructures

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "Nil" should "represent an empty List." in {
    Nil should equal(List[Nothing]())
  }

  "Cons" should "represent a List with its head and tail." in {
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
    pending
  }

  "foldLeftByFoldRight" should "accumulate the List from Left by applying given function." in {
    pending
  }

  "appendByFoldRight" should "return a new List one List appended after another." in {
    pending
  }

  "concat" should "return a new List with all given Lists concatenated together." in {
    pending
  }

  "add1" should "return a new List with all elements added by 1 from given List." in {
    pending
  }

  "doubleToString" should "return a new List with all String elements converted from given Double List." in {
    pending
  }

  "map" should "return a new List by applying given function on each element of input List." in {
    pending
  }

  "map2" should "return a new List by applying given function on each element of input List." in {
    pending
  }

  "map3" should "return a new List by applying given function on each element of input List." in {
    pending
  }

  "filter" should "return a new List with elements from given List meeting given condition. " in {
    pending
  }

  "filter2" should "return a new List with elements from given List meeting given condition. " in {
    pending
  }

  "filter3" should "return a new List with elements from given List meeting given condition. " in {
    pending
  }

  "flatMap" should "return a new List by concatenating all Lists generated by splitting each element of input List." in {
    pending
  }

  "filterByFlatMap" should "return a new List with elements from given List meeting given condition. " in {
    pending
  }

  "addPairwise" should "" in {
    pending
  }

  "zipWith" should "" in {
    pending
  }

  "startsWith" should "" in {
    pending
  }

  "hasSubsequence" should "" in {
    pending
  }
  
}
