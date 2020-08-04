package exercise

object MethodNotations extends App {
  val ragul = new Person("Ragul", "Super Delux", 23)
  println(ragul + "RockStar")
  println(+ragul)
  println(ragul.learnsScala)
  println(ragul(4))
}

class Person(name: String, favouriteMovie: String, age: Int) {

  // Change Name
  def unary_+(nickName: String): Person = new Person(name = s"$name ($nickName)",
    favouriteMovie = favouriteMovie,
    age = age)

  // Change Age
  def unary_+ : Person = new Person(name, favouriteMovie, age + 1)

  // Learns
  def learns(topic : String ) : String = s"$name learns $topic"
  def learnsScala: String = learns("Scala")

  // Apply
  def apply(num : Int) : String = s"$name watched $favouriteMovie $num time(s)"
  def apply() : String = s"$name favourite movie is $favouriteMovie"

}
