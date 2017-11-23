package com.fluency03.fpscala.handlingerrors

import org.scalatest._

import scala.util.Try

class OptionTest extends FlatSpec with Matchers {

  "A Some" should "be an Option." in {
    Some(1) shouldBe an [Option[_]]
  }

  "A None" should "be an Option." in {
    None shouldBe an [Option[Nothing]]
  }

  "map" should "apply f if the Option is not None, or None if None." in {
    val f = (s: String) => s + "-mapped"
    None.map(f) should equal(None)
    Some("some").map(f) should equal(Some("some-mapped"))
  }

  "getOrElse" should "get value if None, or None if None." in {
    val d = Some("else")
    None.getOrElse(d) should equal(Some("else"))
    Some("some").getOrElse(d) should equal("some")
  }

  "flatMap" should "apply flatMap function if not None, or return None." in {
    val f = (s: String) => Some(s + "-mapped")
    None.flatMap(f) should equal(None)
    Some("some").flatMap(f) should equal(Some("some-mapped"))
  }

  "flatMap2" should "apply flatMap function if not None, or return None." in {
    val f = (s: String) => Some(s + "-mapped")
    None.flatMap2(f) should equal(None)
    Some("some").flatMap2(f) should equal(Some("some-mapped"))
  }

  "orElse" should "return itself if not None, otherwise None." in {
    val d = Some("else")
    None.orElse(d) should equal(Some("else"))
    Some("some").orElse(d) should equal(Some("some"))
  }

  "orElse2" should "return itself if not None, otherwise None." in {
    val d = Some("else")
    None.orElse2(d) should equal(Some("else"))
    Some("some").orElse2(d) should equal(Some("some"))
  }

  "filter" should "convert Some to None if the value doesn’t satisfy f." in {
    val isZero = (i: Int) => i == 0
    None.filter(isZero) should equal(None)
    Some(1).filter(isZero) should equal(None)
    Some(0).filter(isZero) should equal(Some(0))
  }

  "filter2" should "convert Some to None if the value doesn’t satisfy f." in {
    val isZero = (i: Int) => i == 0
    None.filter2(isZero) should equal(None)
    Some(1).filter2(isZero) should equal(None)
    Some(0).filter2(isZero) should equal(Some(0))
  }

  "variance" should "return the variance of a sequence of Doubles." in {
    Option.variance(Seq()) should equal(None)
    Option.variance(Seq(1.0)) should equal(Some(0.0))
    Option.variance(Seq(1.0, 2.0)) should equal(Some(0.25))
  }

  "mean" should "return the mean value of a sequence of Doubles." in {
    Option.mean(Seq()) should equal(None)
    Option.mean(Seq(1.0)) should equal(Some(1.0))
    Option.mean(Seq(1.0, 2.0)) should equal(Some(1.5))
  }

  "lift" should "return a lift function given map function on normal value." in {
    pending
  }

  "map2" should "apply f if the Option is not None, or None if None." in {
    val f = (a: Int, b: Int) => a + b
    Option.map2(Some(1), Some(2))(f) should equal(Some(3))
    Option.map2(None, Some(2))(f) should equal(None)
    Option.map2(Some(1), None)(f) should equal(None)
    Option.map2(None, None)(f) should equal(None)
  }

  "map22" should "apply f if the Option is not None, or None if None." in {
    val f = (a: Int, b: Int) => a + b
    Option.map22(Some(1), Some(2))(f) should equal(Some(3))
    Option.map22(None, Some(2))(f) should equal(None)
    Option.map22(Some(1), None)(f) should equal(None)
    Option.map22(None, None)(f) should equal(None)
  }

  "insuranceRateQuote" should "" in {
    pending
  }

  "Try" should "return Some if no Exception, otherwise None." in {
    Option.Try(0 / 0) should equal(None)
    Option.Try(1 / 1) should equal(Some(1))
  }

  "parseInsuranceRateQuote" should "" in {
    pending
  }

  "sequence" should "convert a List of Options to an Option of a List." in {
    Option.sequence(Nil) should equal(Some(Nil))
    Option.sequence(List(Some(1))) should equal(Some(List(1)))
    Option.sequence(List(Some(1), None)) should equal(None)
  }

  "sequence2" should "" in {
    Option.sequence2(Nil) should equal(Some(Nil))
    Option.sequence2(List(Some(1))) should equal(Some(List(1)))
    Option.sequence2(List(Some(1), None)) should equal(None)
  }

  "parseInts" should "convert a List of Strings to an Option of a List of Ints." in {
    Option.parseInts(Nil) should equal(Some(Nil))
    Option.parseInts(List("1", "2", "3")) should equal(Some(List(1, 2, 3)))
    Option.parseInts(List("a", "2", "3")) should equal(None)
  }

  "traverse" should "convert a List of A to an Option of List of B." in {
    val f: String => Option[Int] = (s: String) => Option.Try(s.toInt)
    Option.traverse(Nil)(f) should equal(Some(Nil))
    Option.traverse(List("1", "2", "3"))(f) should equal(Some(List(1, 2, 3)))
    Option.traverse(List("a", "2", "3"))(f) should equal(None)
  }

  "traverse2" should "convert a List of A to an Option of List of B." in {
    val f: String => Option[Int] = (s: String) => Option.Try(s.toInt)
    Option.traverse2(Nil)(f) should equal(Some(Nil))
    Option.traverse2(List("1", "2", "3"))(f) should equal(Some(List(1, 2, 3)))
    Option.traverse2(List("a", "2", "3"))(f) should equal(None)
  }

  "sequenceViaTraverse" should "convert a List of Options to an Option of a List." in {
    Option.sequenceViaTraverse(Nil) should equal(Some(Nil))
    Option.sequenceViaTraverse(List(Some(1))) should equal(Some(List(1)))
    Option.sequenceViaTraverse(List(Some(1), None)) should equal(None)
  }

}
