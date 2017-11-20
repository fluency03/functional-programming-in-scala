package com.fluency03.fpscala.gettingstarted

import org.scalatest._

class PolymorphicFunctionsTest extends FlatSpec with Matchers {

  it should "find the first matching element in the Array." in {
    val array = Array("a", "b", "c", "c")
    PolymorphicFunctions.findFirst(array, (e: String) => e == "b") should equal(1)
    PolymorphicFunctions.findFirst(array, (e: String) => e == "c") should equal(2)
    PolymorphicFunctions.findFirst(array, (e: String) => e == "d") should equal(-1)
  }

  it should "check whether an Array is sorted based on given function." in {
    val largerOrEqualThan = (e1: Int, e2: Int) => e2 >= e1

    val array1 = Array(1, 2, 3, 3, 4)
    PolymorphicFunctions.isSorted(array1, largerOrEqualThan) should equal(true)

    val array2 = Array(1, 2, 3, 4, 3)
    PolymorphicFunctions.isSorted(array2, largerOrEqualThan) should equal(false)
  }










}
