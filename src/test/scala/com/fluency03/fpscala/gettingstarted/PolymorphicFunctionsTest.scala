package com.fluency03.fpscala.gettingstarted

import org.scalatest._

class PolymorphicFunctionsTest extends FlatSpec with Matchers {

  "binarySearch" should "find the first matching element in the Array." in {
    val array = Array[Double](1.0, 2.0, 3.0, 4.0)
    val gt = (e: Double, key: Double) => e > key
    PolymorphicFunctions.binarySearch(array, 2.0, gt) should equal(1)
    PolymorphicFunctions.binarySearch(array, 3.0, gt) should equal(2)
    PolymorphicFunctions.binarySearch(array, 0.0, gt) should equal(-1)
    PolymorphicFunctions.binarySearch(array, 5.0, gt) should equal(-1)
  }

  "findFirst" should "find the first matching element in the Array." in {
    val array = Array[String]("a", "b", "c", "c")
    PolymorphicFunctions.findFirst(array, (e: String) => e == "b") should equal(1)
    PolymorphicFunctions.findFirst(array, (e: String) => e == "c") should equal(2)
    PolymorphicFunctions.findFirst(array, (e: String) => e == "d") should equal(-1)
    PolymorphicFunctions.findFirst(array, (e: String) => e == "0") should equal(-1)
  }

  "isSorted" should "check whether an Array is sorted based on given function." in {
    val largerOrEqualThan = (e1: Int, e2: Int) => e2 >= e1

    val array1 = Array(1, 2, 3, 3, 4)
    PolymorphicFunctions.isSorted(array1, largerOrEqualThan) should equal(true)

    val array2 = Array(1, 2, 3, 4, 3)
    PolymorphicFunctions.isSorted(array2, largerOrEqualThan) should equal(false)
  }

  // TODO (fluency03)
  "partial1" should "return a partial function." in {
    pending
  }

  "curry" should "return a currying function." in {
    pending
  }

  "uncurry" should "return an uncurried function." in {
    pending
  }

  "compose" should "return a composed function." in {
    pending
  }
}
