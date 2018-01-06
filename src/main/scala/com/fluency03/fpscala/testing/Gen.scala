package com.fluency03.fpscala.testing

import com.fluency03.fpscala.state._
import com.fluency03.fpscala.testing.Prop.{FailedCase, SuccessCount}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = ???


  def listOfN(size: Gen[Int]): Gen[List[A]] = ???


}

object Gen {

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /* Write choose as an explicit state action, but this is far less
   convenient, since it requires us to manually thread the `RNG` through the
   computation. */
  def choose2(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => RNG.nonNegativeInt(rng) match {
      case (n, rng2) => (start + n % (stopExclusive - start), rng2)
    }))

  def unit[A](a: => A): Gen[A] = ???

  def boolean: Gen[Boolean] = ???

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???

}


trait Prop {
  def check: Boolean = ???

//  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }



}


object Prop {
  type FailedCase = String
  type SuccessCount = Int



}

