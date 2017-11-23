package com.fluency03.fpscala.handlingerrors

import org.scalatest._

class EitherTest extends FlatSpec with Matchers {

  "A Left" should "be an Either." in {
    Left("e") shouldBe an [Either[_, _]]
  }

  "A Right" should "be an Either." in {
    Right("v") shouldBe an [Either[_, _]]
  }

  "map" should "apply map function if Right, or return original Left." in {
    val f = (s: String) => s + "-mapped"
    Left("e").map(f) should equal(Left("e"))
    Right("v").map(f) should equal(Right("v-mapped"))
  }

  "flatMap" should "apply flatMap function if Right, or return original Left." in {
    val f = (s: String) => Right(s + "-mapped")
    Left("e").flatMap(f) should equal(Left("e"))
    Right("v").flatMap(f) should equal(Right("v-mapped"))
  }

  "orElse" should "return it self if Right, or another Right if Left." in {
    val b = Right("else")
    Left("e").orElse(b) should equal(Right("else"))
    Right("v").orElse(b) should equal(Right("v"))
  }

  "map2" should "apply map function if all Right, or return original Left if at least one Left." in {
    val b = Right("else")
    val bLeft = Left("else")
    val f = (a: String, b: String) => a + "&" + b
    Left("e").map2(b)(f) should equal(Left("e"))
    Left("e").map2(bLeft)(f) should equal(Left("e"))
    Right("v").map2(b)(f) should equal(Right("v&else"))
    Right("v").map2(bLeft)(f) should equal(Left("else"))
  }

  "mean" should "return Right of mean value of a sequence, or Left of error if sequence is empty." in {
    Either.mean(IndexedSeq()) should equal(Left("mean of empty list!"))
    Either.mean(IndexedSeq(1.0)) should equal(Right(1.0))
    Either.mean(IndexedSeq(1.0, 2.0, 3.0)) should equal(Right(2.0))
  }

  "safeDiv" should "return Right of division of two ints, or Left if ArithmeticException." in {
    Either.safeDiv(1, 1) should equal(Right(1))
    Either.safeDiv(0, 1) should equal(Right(0))
    Either.safeDiv(1, 2) should equal(Right(0))
    Either.safeDiv(2, 1) should equal(Right(2))
    Either.safeDiv(1, 0) shouldBe a [Left[_]]
    Either.safeDiv(1, 0)
      .asInstanceOf[Left[_]]
      .value shouldBe a [ArithmeticException]
    Either.safeDiv(1, 0)
      .asInstanceOf[Left[_]]
      .value
      .asInstanceOf[ArithmeticException]
      .getMessage should equal("/ by zero")
  }

  "Try" should "return corrct Right value if not Exception, or Left with Exception." in {
    Either.Try(2/1) should equal(Right(2))
    Either.Try(2/0) shouldBe a [Left[_]]
  }

  "sequence" should "" in {
    pending
  }

  "sequence2" should "" in {
    pending
  }

  "traverse" should "" in {
    pending
  }

  "traverse2" should "" in {
    pending
  }
}
