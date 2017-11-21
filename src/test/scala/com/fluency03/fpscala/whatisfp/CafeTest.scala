package com.fluency03.fpscala.whatisfp

import org.scalatest._

class CafeTest extends FlatSpec with Matchers {

  "A Coffee" should "have a default price 3.0." in {
    Coffee().price should equal (3.0)
  }

  it should "buy a Coffee with a Credit Card." in {
    val cc = CreditCard("cc")

    val cafe = new Cafe
    val (coffee, charge) = cafe.buyCoffee(cc)

    coffee should equal(Coffee())
    charge should equal(Charge(cc, coffee.price))

  }

  it should "buy multiple Coffees with a Credit Card." in {
    val cc = CreditCard("cc")

    val cafe = new Cafe
    val (coffees, charge) = cafe.buyCoffees(cc, 5)

    coffees should have length 5
    coffees foreach {
      c => c should equal(Coffee())
    }

    charge should equal(Charge(CreditCard("cc"), Coffee().price * 5))
  }

}
