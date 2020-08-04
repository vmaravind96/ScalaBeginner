package exercise

import scala.annotation.tailrec

object Functions extends App {

  def greetFunction(name: String, age: Int): String = {
    "My name is " + name + " and age is " + age
  }

  println(greetFunction("Aravind", 23))

  def factorial(n: Int): Int = {
    if (n == 1) 1
    else n * factorial(n - 1)
  }

  println(factorial(5))

  def tailFactorial(n: BigInt): BigInt = {
    @tailrec
    def factHelper(x: BigInt, accum: BigInt = 1): BigInt = {
      if (x == 1) accum
      else factHelper(x-1, x * accum)
    }
    factHelper(n, 1)
  }
  println(tailFactorial(10000))

  def concatString(s : String, n : Int): String = {
    @tailrec
    def concatHelper(n : Int, s: String , accum : String): String = {
      if (n==0) accum
      else concatHelper(n-1, s ,accum+s)
    }
    concatHelper(n, s, "")
  }
  println(concatString("Hai", 3))
}
