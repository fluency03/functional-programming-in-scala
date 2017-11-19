package com.fluency03.fpscala.whatisfp

import org.scalatest._

class CafeTest extends FlatSpec with Matchers {

  it should " be able to buy a Coffee with a Credit Card from a Cafe." in {
    val cc = CreditCard("cc")

    val cafe = new Cafe
    val (coffee, charge) = cafe.buyCoffee(cc)

    coffee should equal(Coffee())
    charge should equal(Charge(cc, coffee.price))

  }

  it should " be able to buy 5 Coffees with a Credit Card from a Cafe." in {
    val cc = CreditCard("cc")

    val cafe = new Cafe
    val (coffees, charge) = cafe.buyCoffees(cc, 5)

    coffees should have length 5
    coffees foreach {
      c => c should equal(Coffee())
    }

    charge should equal(Charge(cc, Coffee().price * 5))

  }



}
