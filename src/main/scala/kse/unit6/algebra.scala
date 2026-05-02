package kse.unit6

import kse.unit4.challenge.numerals.{Numeral, Zero}
import scala.annotation.targetName

object algebra:

  trait Semigroup[V]:
    def combine(left: V, right: V): V

  object Semigroup:
    def apply[V](using semigroup: Semigroup[V]): Semigroup[V] = semigroup

  trait Monoid[V] extends Semigroup[V]:
    def unit: V

  object Monoid:
    def apply[V](using monoid: Monoid[V]): Monoid[V] = monoid

  extension [V: Monoid as monoid](elem: V) infix def +(that: V): V = monoid.combine(elem, that)

  given Monoid[Int] with
    def combine(left: Int, right: Int): Int = left + right
    def unit: Int                           = 0

  given Monoid[String] with
    def combine(left: String, right: String): String = left.concat(right)
    def unit: String                                 = ""

  given [V]: Monoid[List[V]] with
    def combine(left: List[V], right: List[V]): List[V] = left ::: right
    def unit: List[V]                                   = Nil

  given Monoid[Numeral] with
    def combine(left: Numeral, right: Numeral): Numeral = left + right
    def unit: Numeral                                   = Zero

  def fold[T: Monoid](list: List[T]): T =
    list match
      case Nil          => Monoid[T].unit
      case head :: tail => Monoid[T].combine(head, fold(tail))

  def sum(list: List[Int]): Int = fold(list)

  def concat(list: List[String]): String = fold(list)

  case class Code(content: String)

  given Monoid[Code] with
    def combine(left: Code, right: Code): Code = right
    def unit: Code                             = Code("")

  def merge(list: List[Code]): Code = fold(list)
