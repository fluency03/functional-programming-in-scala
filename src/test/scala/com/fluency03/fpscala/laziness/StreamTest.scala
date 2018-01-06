package com.fluency03.fpscala.laziness

import org.scalatest._

class StreamTest extends FlatSpec with Matchers with BeforeAndAfter {

  var headOne: () => Int = _
  var headTwo: () => Int = _
  var emptyStream: Stream[Int] = _
  var emptyTail: () => Stream[Int] = _
  var singleHeadCons: Cons[Int] = _
  var nonEmptyTail: () => Stream[Int] = _
  var twoCons: Cons[Int] = _

  before {
    headOne = () => 1
    headTwo = () => 2
    emptyStream = Empty
    emptyTail = () => emptyStream
    singleHeadCons = Cons(headOne, emptyTail)
    nonEmptyTail = () => singleHeadCons
    twoCons = Cons(headTwo, nonEmptyTail)
  }

  "An Empty" should "represent an empty Stream." in {
    Stream.empty should equal(Empty)
    Empty should equal(Stream[Nothing]())
  }

  "A Cons" should "represent a Stream with its head and tail." in {
    singleHeadCons shouldBe a [Stream[_]]
    singleHeadCons.h should equal(headOne)
    singleHeadCons.t should equal(emptyTail)
    twoCons shouldBe a [Stream[_]]
    twoCons.h should equal(headTwo)
    twoCons.t should equal(nonEmptyTail)
  }

  "headOption" should "return the Option version of the Stream head." in {
    Empty.headOption should equal(None)
    singleHeadCons.headOption should equal(Some(1))
    twoCons.headOption should equal(Some(2))
  }

  "toList" should "return the List version of a Stream" in {
    Empty.toList should equal(Nil)
    singleHeadCons.toList should equal(List(1))
    twoCons.toList should equal(List(2, 1))
    Empty.toListTailRec should equal(Nil)
    singleHeadCons.toListTailRec should equal(List(1))
    twoCons.toListTailRec should equal(List(2, 1))
    Empty.toListFast should equal(Nil)
    singleHeadCons.toListFast should equal(List(1))
    twoCons.toListFast should equal(List(2, 1))
  }

  "d" should "" in {
    pending
  }





}
