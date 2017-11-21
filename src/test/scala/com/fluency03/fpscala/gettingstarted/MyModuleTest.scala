package com.fluency03.fpscala.gettingstarted

import org.scalatest._

class MyModuleTest extends FlatSpec with Matchers {

  "abs" should "return the absolute value of given integer." in {
    MyModule.abs(1) should equal(1)
    MyModule.abs(-1) should equal(1)
    MyModule.abs(0) should equal(0)
  }

  "factorial" should "return the factorial result of given integer." in {
    MyModule.factorial(7) should equal(5040)
    MyModule.factorial(1) should equal(1)
    MyModule.factorial(0) should equal(1)
    MyModule.factorial(-1) should equal(1)
    MyModule.factorial(-7) should equal(1)
    MyModule.factorial(-10) should equal(1)
  }

  "fib" should "return the nth Fibonacci number of given integer n." in {
    MyModule.fib(7) should equal(13)
    MyModule.fib(10) should equal(55)
    MyModule.fib(1) should equal(1)
    MyModule.fib(2) should equal(1)

    the [IllegalArgumentException] thrownBy {
      MyModule.fib(0)
    } should have message "Input number should be larger than zero."

    the [IllegalArgumentException] thrownBy {
      MyModule.fib(-1)
    } should have message "Input number should be larger than zero."
  }

  "formatResult" should "return the result based on given function." in {
    MyModule.formatResult("absolute value", -42, MyModule.abs) should equal("The absolute value of -42 is 42.")
    MyModule.formatResult("factorial", 7, MyModule.factorial) should equal("The factorial of 7 is 5040.")
    MyModule.formatResult("Fibonacci number", 10, MyModule.fib) should equal("The Fibonacci number of 10 is 55.")
  }

}
