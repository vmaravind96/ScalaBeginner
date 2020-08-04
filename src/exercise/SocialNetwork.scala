package exercise

import scala.annotation.tailrec

object SocialNetwork extends App {

  // Map
  println("***************** Sample Testing ***************************")
  val phonebook: Map[String, Int] = Map(("Aravind", 123), ("aravind", 567), ("Amirtha", 456))
  println(phonebook)
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2))

  // Social Network
  def addPerson(network: Map[String, Set[String]], newPerson: String): Map[String, Set[String]] = {
    network + (newPerson -> Set())
  }

  def friend(network: Map[String, Set[String]], A: String, B: String): Map[String, Set[String]] = {
    val friendsA = network(A)
    val friendsB = network(B)
    network + (A -> (friendsA + B)) + (B -> (friendsB + A))
  }

  def unfriend(network: Map[String, Set[String]], A: String, B: String): Map[String, Set[String]] = {
    val friendsA = network(A)
    val friendsB = network(B)
    network + (A -> (friendsA - B)) + (B -> (friendsB - A))
  }

  def removePerson(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {

    @tailrec
    def removeLinks(friendsList: Set[String], networkAccum: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (friendsList.isEmpty) networkAccum
      else removeLinks(friendsList.tail, unfriend(networkAccum, person, friendsList.head))
    }

    val unfriendedNetwork = removeLinks(network(person), network)
    unfriendedNetwork - person
  }

  def numberOfFriends(network: Map[String, Set[String]], person: String): Int = {
    if (!network.contains(person)) 0
    else network(person).size
  }

  def maxFriends(network: Map[String, Set[String]]): String = {
    network.maxBy(pair => pair._2.size)._1
  }

  def noFriends(network: Map[String, Set[String]]): Int = {
    network.count(pair => pair._2.isEmpty)
  }

  def socialConnection(network: Map[String, Set[String]], A: String, B: String): Boolean = {
    @tailrec
    def checkConnection(friendList: Set[String], person: String): Boolean = {
      if (friendList.isEmpty) false
      else if (network(friendList.head).contains(person)) true
      else checkConnection(friendList.tail, person)
    }

    checkConnection(network(A), B) || checkConnection(network(B), A)
  }

  println("")
  println("***************** Social Networking ***************************")
  println("")

  // Create Network
  var initialNetwork: Map[String, Set[String]] = Map()
  initialNetwork = addPerson(initialNetwork, "Aravind")
  initialNetwork = addPerson(initialNetwork, "Amirtha")
  initialNetwork = addPerson(initialNetwork, "Mohan")
  initialNetwork = addPerson(initialNetwork, "Vijayasri")
  initialNetwork = addPerson(initialNetwork, "Crypto")
  initialNetwork = addPerson(initialNetwork, "Crypto1")
  initialNetwork = addPerson(initialNetwork, "TestUser")

  // Add Friends
  initialNetwork = friend(initialNetwork, "Aravind", "Mohan")
  initialNetwork = friend(initialNetwork, "Aravind", "Amirtha")
  initialNetwork = friend(initialNetwork, "Aravind", "Vijayasri")
  initialNetwork = friend(initialNetwork, "Aravind", "Crypto")
  initialNetwork = friend(initialNetwork, "Amirtha", "Crypto")
  initialNetwork = friend(initialNetwork, "Amirtha", "Crypto1")
  initialNetwork = friend(initialNetwork, "Aravind", "Crypto1")
  println(initialNetwork)

  // Remove Friends
  initialNetwork = unfriend(initialNetwork, "Aravind", "Crypto1")
  println(initialNetwork)

  // Remove Person
  initialNetwork = removePerson(initialNetwork, "Crypto")
  println(initialNetwork)

  // Number of Friends
  println(numberOfFriends(initialNetwork, "Amirtha"))

  // Max Friends
  println(maxFriends(initialNetwork))

  // No Friends
  println(noFriends(initialNetwork))

  // Social Connection
  println(socialConnection(initialNetwork, "Aravind", "Crypto1"))
  println(socialConnection(initialNetwork, "Aravind", "TestUser"))
}
