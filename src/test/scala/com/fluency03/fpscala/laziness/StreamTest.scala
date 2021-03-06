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

  "cons" should "concentrate a head and a tail to a Stream." in {
    val cons1 = Stream.cons(1, emptyStream)
    cons1 shouldBe a [Stream[_]]
    cons1 shouldBe a [Cons[_]]
    val newCons = cons1.asInstanceOf[Cons[Int]]
    newCons.headOption should equal(Some(1))
    newCons.t() should equal(emptyStream)
  }

  "apply" should "be able to construct a new Stream." in {
    Stream() should equal(Empty)
    val s = Stream(3, 2, 1)
    s shouldBe a [Stream[_]]
    s shouldBe a [Cons[_]]
    val newCons = s.asInstanceOf[Cons[Int]]
    newCons.headOption should equal(Some(3))
    newCons.t().headOption should equal(Some(2))
    val subCons = newCons.t().asInstanceOf[Cons[Int]]
    subCons.t().headOption should equal(Some(1))
    s.toList should equal(List(3, 2, 1))
  }

  "headOption" should "return the Option version of the Stream head." in {
    Empty.headOption should equal(None)
    singleHeadCons.headOption should equal(Some(1))
    twoCons.headOption should equal(Some(2))

    Empty.headOptionByFoldRight should equal(None)
    singleHeadCons.headOptionByFoldRight should equal(Some(1))
    twoCons.headOptionByFoldRight should equal(Some(2))
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

  "take" should "return the first n element from a Stream." in {
    Empty.take(0) should equal(Empty)
    Empty.take(1) should equal(Empty)
    singleHeadCons.take(0) should equal(Empty)
    singleHeadCons.take(1).toList should equal(List(1))
    twoCons.take(0) should equal(Empty)
    twoCons.take(1).toList should equal(List(2))
    twoCons.take(2).toList should equal(List(2, 1))
    Stream(3, 2, 1).take(2).toList should equal(List(3, 2))

    Empty.takeByUnfold(0) should equal(Empty)
    Empty.takeByUnfold(1) should equal(Empty)
    singleHeadCons.takeByUnfold(0) should equal(Empty)
    singleHeadCons.takeByUnfold(1).toList should equal(List(1))
    twoCons.takeByUnfold(0) should equal(Empty)
    twoCons.takeByUnfold(1).toList should equal(List(2))
    twoCons.takeByUnfold(2).toList should equal(List(2, 1))
    Stream(3, 2, 1).takeByUnfold(2).toList should equal(List(3, 2))
  }

  "drop" should "drop the first n element from a Stream." in {
    Empty.drop(0) should equal(Empty)
    Empty.drop(1) should equal(Empty)
    singleHeadCons.drop(0) should equal(singleHeadCons)
    singleHeadCons.drop(1) should equal(Empty)
    twoCons.drop(0) should equal(twoCons)
    twoCons.drop(1) should equal(singleHeadCons)
    twoCons.drop(2) should equal(Empty)
    Stream(3, 2, 1).drop(2).toList should equal(List(1))
  }

  "takeWhile" should "return all starting elements of a Stream that match the given predicate." in {
    val p0 = (n: Int) => n >= 1
    Empty.takeWhile(p0) should equal(Empty)
    singleHeadCons.takeWhile(_ < 1) should equal(Empty)
    singleHeadCons.takeWhile(p0).toList should equal(List(1))
    twoCons.takeWhile(_ > 2) should equal(Empty)
    twoCons.takeWhile(_ > 1).toList should equal(List(2))
    twoCons.takeWhile(p0).toList should equal(List(2, 1))
    Stream(3, 2, 1).takeWhile(_ > 1).toList should equal(List(3, 2))

    Empty.takeWhileByFoldRight(p0) should equal(Empty)
    singleHeadCons.takeWhileByFoldRight(_ < 1) should equal(Empty)
    singleHeadCons.takeWhileByFoldRight(p0).toList should equal(List(1))
    twoCons.takeWhileByFoldRight(_ > 2) should equal(Empty)
    twoCons.takeWhileByFoldRight(_ > 1).toList should equal(List(2))
    twoCons.takeWhileByFoldRight(p0).toList should equal(List(2, 1))
    Stream(3, 2, 1).takeWhileByFoldRight(_ > 1).toList should equal(List(3, 2))

    Empty.takeWhileByUnfold(p0) should equal(Empty)
    singleHeadCons.takeWhileByUnfold(_ < 1) should equal(Empty)
    singleHeadCons.takeWhileByUnfold(p0).toList should equal(List(1))
    twoCons.takeWhileByUnfold(_ > 2) should equal(Empty)
    twoCons.takeWhileByUnfold(_ > 1).toList should equal(List(2))
    twoCons.takeWhileByUnfold(p0).toList should equal(List(2, 1))
    Stream(3, 2, 1).takeWhileByUnfold(_ > 1).toList should equal(List(3, 2))
  }

  "exists" should "checks whether an element matching a Boolean function exists." in {
    Empty.exists(_ == 1) should equal(false)
    singleHeadCons.exists(_ < 1) should equal(false)
    singleHeadCons.exists(_ == 1) should equal(true)
    twoCons.exists(_ > 2) should equal(false)
    twoCons.exists(_ > 1) should equal(true)
    twoCons.exists(_ == 1) should equal(true)
    Stream(3, 2, 1).exists(_ > 1) should equal(true)

    Empty.existsByFoldRight(_ == 1) should equal(false)
    singleHeadCons.existsByFoldRight(_ < 1) should equal(false)
    singleHeadCons.existsByFoldRight(_ == 1) should equal(true)
    twoCons.existsByFoldRight(_ > 2) should equal(false)
    twoCons.existsByFoldRight(_ > 1) should equal(true)
    twoCons.existsByFoldRight(_ == 1) should equal(true)
    Stream(3, 2, 1).existsByFoldRight(_ > 1) should equal(true)
  }

  "foldRight" should "apply a fold function on a Stream, from right." in {
    Empty.foldRight(1)((a: Int, b) => a + b) should equal(1)
    singleHeadCons.foldRight(1)((a: Int, b) => a + b) should equal(2)
    singleHeadCons.foldRight(0)((a: Int, b) => a + b) should equal(1)
    twoCons.foldRight(1)((a: Int, b) => a + b) should equal(4)
    Stream(3, 2, 1).foldRight(1)((a: Int, b) => a + b) should equal(7)

    Empty.foldRight(List(1))((a: Int, b) => a :: b) should equal(List(1))
    singleHeadCons.foldRight(List(0))((a: Int, b) => a :: b) should equal(List(1, 0))
    singleHeadCons.foldRight(List(1))((a: Int, b) => a :: b) should equal(List(1, 1))
    twoCons.foldRight(List(1))((a: Int, b) => a :: b) should equal(List(2, 1, 1))
    Stream(3, 2, 1).foldRight(List(1))((a: Int, b) => a :: b) should equal(List(3, 2, 1, 1))
  }





}
