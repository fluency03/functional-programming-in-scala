package com.fluency03.fpscala.whatisfp

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cupOfCoffee = Coffee()
    (cupOfCoffee, Charge(cc, cupOfCoffee.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

}

case class Coffee(price: Double = 3.0)
