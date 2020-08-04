package exercise

object InbuiltList extends App {

  // Product
  val L1 = List(1, 2, 3)
  val L2 = List('a', 'b', 'c')

  println(L1.flatMap(n => L2.map(ch => s"$n$ch")))
}
