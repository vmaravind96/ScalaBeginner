package exercise

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  def printElements: String

  override def toString: String = s"[$printElements]"

  def map[B](transform: A => B): MyList[B]

  def filter(predicate: A => Boolean): MyList[A]

  def flatMap[B](transform: A => MyList[B]): MyList[B]

  def ++[B >: A](list: MyList[B]): MyList[B]

  def forEach(f : A=>Unit): Unit

  def sort(f: (A, A) => Int): MyList[A]

  def zipWith[B, C](list : MyList[B], zip : (A, B)=>C) : MyList[C]

  def fold[B](value: B)(f : (A, B) => B ): B
}

// Empty List
object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def add[B >: Nothing](element: B): MyList[B] = new List(element, Empty)

  override def printElements: String = ""

  def map[B](transform: Nothing => B): MyList[B] = Empty

  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def flatMap[B](transform: Nothing => MyList[B]): MyList[B] = Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  def forEach(f : Nothing => Unit): Unit = ()

  def sort(f: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

  def zipWith[B, C](list : MyList[B], zip : (Nothing, B)=>C) : MyList[C] = {
    if (!list.isEmpty) throw new Exception("List is not empty")
    else Empty
  }

  def fold[B](value: B)(f : (Nothing, B) => B ): B = value
}

// Non Empty List
class List[+A](val h: A, val t: MyList[A]) extends MyList[A] {
  def head: A = h

  def tail: MyList[A] = t

  def isEmpty: Boolean = false

  def add[B >: A](element: B): MyList[B] = new List(element, this)

  override def printElements: String = {
    if (t.isEmpty) s"$h"
    else s"$h ${t.printElements}"
  }

  def filter(predicate: A => Boolean): MyList[A] = {
    if (predicate(h)) new List(h, t.filter(predicate))
    else t.filter(predicate)
  }

  def map[B](transform: A => B): MyList[B] = {
    new List(transform(h), t.map(transform))
  }

  def flatMap[B](transform: A => MyList[B]): MyList[B] = {
    transform(h) ++ t.flatMap(transform)
  }

  def ++[B >: A](list: MyList[B]): MyList[B] = {
    new List(h, t ++ list)
  }

  def forEach(f : A=>Unit): Unit = {
    f(h)
    t.forEach(f)
  }

  def sort(compare: (A, A) => Int): MyList[A] = {
    def insert(x : A, sortedList : MyList[A]): MyList[A] = {
      if (sortedList.isEmpty) new List(x, Empty)
      else if (compare(x, sortedList.head) <= 0) new List(x , sortedList)
      else new List(sortedList.head, insert(x, sortedList.tail))
    }
    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] = {
    if (list.isEmpty && h == Empty) Empty
    else if (list.isEmpty ^ h==Empty) throw new Exception("Size mismatch")
    else new List(zip(h, list.head), t.zipWith(list.tail, zip))
  }

  def fold[B](initialValue: B)(f: (A, B) => B): B = {
    t.fold(f(h , initialValue))(f)
  }
}



//class EvenPredicate extends List[Int] with MyPredicate[Int]{
//  override def test(value : Int) value%2 == 0
//}


object MyListTest extends App {
  val list = new List[Int](5, Empty)
  println(list.head)
  println(list.tail)
  println(list.add(2).head)

  val newList = new List[Int](1, new List(2, new List(3, new List(4, Empty))))
  println(newList.toString)

  val stringList = new List[String]("Hello", new List("I", new List("am", new List("Aravind", Empty))))
  println(stringList.toString)

  println(newList.map(elem => elem*5).toString)

  println(newList.filter(elem => elem%2==0).toString)

  println((list ++ newList).toString)

  println(newList.flatMap(elem => new List(elem, new List(elem+1, Empty))).toString)

  newList.forEach(println)

  println (newList.sort((x :Int, y: Int) => y-x).toString)

  val zipList : MyList[Int] = new List(9, new List(8, new List(7, Empty)))
  try{
    println(newList.zipWith(zipList, (x : Int, y : Int) => x*y).toString)
  } catch {
    case e: Exception => println(e)
  }

  val zipListString : MyList[String] = new List("Ara", new List("Bala", new List("Car", new List("Dog", Empty))))
  try{
    println(newList.zipWith(zipListString, (x : Int, y : String) => s"$x,$y").toString)
  } catch {
    case e: Exception => println(e)
  }

  println(list.fold(0)((x : Int, y: Int) => x+y))
  println(newList.fold(10)((x : Int, y: Int) => x+y))

}
