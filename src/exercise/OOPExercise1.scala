package exercise

object OOPExercise1 extends App {
  val author1 = new Writer("Aravind", lName = "Kumar", 1996)
  val novel1 = new Novel("Pandemic", yor=2020, author = author1 )
  val author2 = new Writer("Murgadoss", lName = "AR", 1980)
  println(author1.getName)
  println(novel1.getAge)
  println(novel1.isWrittenBy(author2))
  println(novel1.copy(2021))
}

class Writer(fName: String, lName: String, val yob: Int) {

  def getName: String = fName + " " + lName

}

class Novel(name: String, yor: Int, author: Writer) {

  def getAge: Int = yor - author.yob

  def isWrittenBy(writer: Writer): Boolean = author == writer

  def copy(newYear: Int): Novel = new Novel(name = name, yor = newYear, author = author)

}


class Counter(count : Int = 0){
  def getCount : Int = count

  def inc: Counter = new Counter(count+1)
  def dec: Counter = new Counter(count-1)

  def inc(x : Int) : Counter = new Counter(count+x)
  def dec(x : Int) : Counter = new Counter(count-x)
}