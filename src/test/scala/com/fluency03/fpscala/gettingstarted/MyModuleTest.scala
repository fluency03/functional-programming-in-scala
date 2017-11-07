package com.fluency03.fpscala.gettingstarted

import org.scalatest.FlatSpec

class MyModuleTest extends FlatSpec {

  "MyModule.abs" should "return the absolute value of given integer." in {
    assert(MyModule.abs(1) == 1)
    assert(MyModule.abs(-1) == 1)
    assert(MyModule.abs(0) == 0)
  }


  "MyModule.factorial" should "return the factorial result of given integer." in {
    assert(MyModule.factorial(7) == 5040)
    assert(MyModule.factorial(1) == 1)
    assert(MyModule.factorial(0) == 1)
    assert(MyModule.factorial(-1) == 1)
    assert(MyModule.factorial(-7) == 1)
    assert(MyModule.factorial(-10) == 1)
  }

  "MyModule.fib" should "return the nth Fibonacci number of given integer n." in {
    assert(MyModule.fib(7) == 13)
    assert(MyModule.fib(10) == 55)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(2) == 1)
    assertThrows[IllegalArgumentException] {
      MyModule.fib(0)
    }
    assertThrows[IllegalArgumentException] {
      MyModule.fib(-1)
    }
  }

  "MyModule.formatResult" should "return the result based on given function." in {
    assert(MyModule.formatResult("absolute value", -42, MyModule.abs) == "The absolute value of -42 is 42.")
    assert(MyModule.formatResult("factorial", 7, MyModule.factorial) == "The factorial of 7 is 5040.")
    assert(MyModule.formatResult("Fibonacci number", 10, MyModule.fib) == "The Fibonacci number of 10 is 55.")
  }

}
