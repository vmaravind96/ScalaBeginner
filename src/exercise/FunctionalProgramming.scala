package exercise

object FunctionalProgramming extends App {

  val concatStrings = new ((String, String) => String) {
    override def apply(v1: String, v2: String): String = s"$v1 $v2"
  }
  println(concatStrings("Apple", "Mango"))

  val actualGetValue = new ((Int) => Int) {
    override def apply(v1: Int): Int = v1 * 2
  }

  val getValue: (Int => (Int => Int)) = x => y => x + y
  println(getValue(3)(5))

  val plusOne = (x:Int) => x+1

  def incrementBetter(f : Int => Int, n : Int): (Int=> Int) =
    if (n<=0) (x : Int) => x
    else (x : Int) => incrementBetter(f, n-1)(f(x))

  val plus10 = incrementBetter(plusOne, 10)
  print(plus10(15))
}

