package com.fluency03.fpscala.datastructures

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

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
















}
