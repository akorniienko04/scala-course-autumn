package kse.unit6

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object order:

  trait Order[T]:
    def compare(left: T, right: T): Int

  extension [V: Order as ord](elem: V)

    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

  object Order:

    given Order[Int] with

      def compare(left: Int, right: Int): Int =
        if left < right then -1
        else if left > right then 1
        else 0

    given Order[Char] with

      def compare(left: Char, right: Char): Int =
        if left < right then -1
        else if left > right then 1
        else 0

    given ListOrder[T](using ord: Order[T]): Order[List[T]] with

      def compare(left: List[T], right: List[T]): Int =
        (left, right) match
          case (Nil, Nil) => 0
          case (Nil, _)   => -1
          case (_, Nil)   => 1
          case (leftHead :: leftTail, rightHead :: rightTail) =>
            val compareHeads = ord.compare(leftHead, rightHead)
            if compareHeads != 0 then compareHeads
            else compare(leftTail, rightTail)

    given StringOrder(using order: Order[List[Char]]): Order[String] with

      def compare(left: String, right: String): Int =
        order.compare(left.toList, right.toList)

  def max[T: Order](left: T, right: T): T =
    if left > right then left else right

//  def max[T](left: T, right: T)(using order: Order[T]): T =
//    if left > right then left else right

  max(1, 2)

  max(List(1, 2, 3), List(4, 5, 6))
  max(List(List("Hello")), List(List("Hello")))

  case class Person(name: String, age: Int)

  given PersonOrder: Order[Person] with

    def compare(left: Person, right: Person): Int =
      summon[Order[Int]].compare(left.age, right.age)

  val p1: Person = Person("John", 20)
  val p2: Person = Person("Max", 22)

  p1 > p2

  max(List(List(p1)), List(List(p2)))

  type Error = String

  trait SafeOrder[T]:
    def compare(left: T, right: T): Either[Error, T]

  given PersonSafeOrder: SafeOrder[Person] with

    def compare(left: Person, right: Person): Either[Error, Person] =
      if left.age < 18 || right.age < 18 then Left("Person is not old enough")
      else if summon[Order[Int]].compare(left.age, right.age) > 1 then Right(left)
      else Right(right)

  def safeMax[T: SafeOrder as ord](left: T, right: T): Either[Error, T] =
    ord.compare(left, right)

  safeMax(p1, p2)
