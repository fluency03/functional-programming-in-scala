package com.fluency03.fpscala.gettingstarted

import org.scalatest._

class MonomorphicBinarySearchTest extends FlatSpec with Matchers {

  "binarySearch" should "find the first matching element in the Array." in {
    val array = Array(1.0, 2.0, 3.0, 4.0)
    MonomorphicBinarySearch.binarySearch(array, 2.0) should equal(1)
    MonomorphicBinarySearch.binarySearch(array, 3.0) should equal(2)
    MonomorphicBinarySearch.binarySearch(array, 0.0) should equal(-1)
    MonomorphicBinarySearch.binarySearch(array, 5.0) should equal(-1)
  }

  "findFirst" should "find the first matching element in the Array." in {
    val array = Array("a", "b", "c", "c")
    MonomorphicBinarySearch.findFirst(array, "b") should equal(1)
    MonomorphicBinarySearch.findFirst(array, "c") should equal(2)
    MonomorphicBinarySearch.findFirst(array, "d") should equal(-1)
    MonomorphicBinarySearch.findFirst(array, "0") should equal(-1)
  }

}
