package exercise

object PatternMatch extends App{

  // Declaration
  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(a : Expr, b: Expr) extends Expr
  case class Prod(a : Expr, b: Expr) extends Expr

  // Pattern Match
  def printNumbers(exp : Expr): String = {
    exp match {
      case Number(n) => s"$n"
      case Prod(a, b) =>
        def paranthesisCheck(e : Expr): String ={
          e match {
            case Sum(_, _) => "(" + printNumbers(e) + ")"
            case _ => printNumbers(e)
          }
        }
        paranthesisCheck(a) + "*" + paranthesisCheck(b)

      case Sum(a, b) => printNumbers(a) + "+" + printNumbers(b)
    }
  }

  // Tests
  println(printNumbers(Number(4)))
  println(printNumbers(Sum(Prod(Number(5), Number(6)), Number(7))))
  println(printNumbers(Prod(Sum(Number(5), Number(6)), Number(7))))
  println(printNumbers(Prod(Sum(Number(5), Number(6)), Sum(Number(3), Number(2)))))
  println(printNumbers(Prod(Sum(Number(5), Number(6)), Prod(Number(3), Number(2)))))

}
