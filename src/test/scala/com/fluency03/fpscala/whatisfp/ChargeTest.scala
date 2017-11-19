package com.fluency03.fpscala.whatisfp

import org.scalatest._

class ChargeTest extends FlatSpec with Matchers  {

  "A Credit Card" should "have its unique name." in {
    CreditCard("cardName").name should equal("cardName")

    CreditCard("cc1") should equal(CreditCard("cc1"))
    CreditCard("cc1") should not equal CreditCard("cc2")
  }

  "Charge 5" should "be able to combine with Charge 10 with same Credit Card." in {
    val cc = CreditCard("cc")

    val charge5 = Charge(cc, 5)
    val charge10 = Charge(cc, 10)

    charge5.combine(charge10) should equal(Charge(cc, 15))
  }

  "Charge 5" should "not be able to combine with Charge 10 with different Credit Card." in {
    val cc = CreditCard("cc")
    val cc2 = CreditCard("cc2")

    val charge5 = Charge(cc, 5)
    val charge10 = Charge(cc2, 10)

    an[IllegalArgumentException] should be thrownBy charge5.combine(charge10)
  }

  it should "be able to coalesce a List of Charges with same Credit Card." in {
    val cc = CreditCard("cc")
    val chargeList = List.fill(5)(Charge(cc, 2))

    Charge.coalesce(chargeList) should equal(List(Charge(cc, 10)))
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
