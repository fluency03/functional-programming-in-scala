package com.fluency03.fpscala.whatisfp

import org.scalatest._

class ChargeTest extends FlatSpec with Matchers  {

  "Charge 5" should "be able to combine with Charge 10." in {
    val cc = CreditCard("cc")

    val c5 = Charge(cc, 5)
    val c10 = Charge(cc, 10)

    c5.combine(c10) should equal(Charge(cc, 15))

    val cc2 = CreditCard("cc2")
    val c8 = Charge(cc2, 8)

    an [IllegalArgumentException] should be thrownBy c5.combine(c8)
  }


  it should "be able to coalesce a List of Charges with same Credit Card." in {
    val cc = CreditCard("cc")
    val cList = List.fill(5)(Charge(cc, 2))

    Charge.coalesce(cList) should equal(List(Charge(cc, 10)))
  }


  it should "be able to coalesce a List of Charges with different Credit Cards." in {
    val cc = CreditCard("cc")
    val cList = List.fill(5)(Charge(cc, 2))

    val cc2 = CreditCard("cc2")
    val cList2 = List.fill(5)(Charge(cc2, 2))

    val cList3 = cList2 ++ cList

    val newList = Charge.coalesce(cList3)
    newList should have length 2
    newList should contain only (Charge(cc, 10), Charge(cc2, 10))
  }




}
