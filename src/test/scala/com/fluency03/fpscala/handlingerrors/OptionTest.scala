package com.fluency03.fpscala.handlingerrors

import org.scalatest._

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

  "getOrElse" should "" in {
    val d = Some("else")
    None.getOrElse(d) should equal(Some("else"))
    Some("some").getOrElse(d) should equal("some")
  }

  "flatMap" should "" in {
    val f = (s: String) => Some(s + "-mapped")
    None.flatMap(f) should equal(None)
    Some("some").flatMap(f) should equal(Some("some-mapped"))
  }

  "flatMap2" should "" in {
    val f = (s: String) => Some(s + "-mapped")
    None.flatMap2(f) should equal(None)
    Some("some").flatMap2(f) should equal(Some("some-mapped"))
  }

  "orElse" should "" in {
    val d = Some("else")
    None.orElse(d) should equal(Some("else"))
    Some("some").orElse(d) should equal(Some("some"))
  }

  "orElse2" should "" in {
    val d = Some("else")
    None.orElse2(d) should equal(Some("else"))
    Some("some").orElse2(d) should equal(Some("some"))
  }

  "filter" should "" in {
    val isZero = (i: Int) => i == 0
    None.filter(isZero) should equal(None)
    Some(1).filter(isZero) should equal(None)
    Some(0).filter(isZero) should equal(Some(0))
  }

  "filter2" should "" in {
    val isZero = (i: Int) => i == 0
    None.filter2(isZero) should equal(None)
    Some(1).filter2(isZero) should equal(None)
    Some(0).filter2(isZero) should equal(Some(0))
  }

  "" should "" in {
    pending
  }



}
