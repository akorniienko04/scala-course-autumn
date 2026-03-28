package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = new Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = (this > that) || (this == that)

    @targetName("less than")
    infix def <(that: Numeral): Boolean = !(this >= that)

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = !(this > that)

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional challenge.
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = this

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that

    // Optional challenge.
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = this

    override def toString: String = "zero"

    override def equals(obj: Any): Boolean = obj match
      case _: Zero.type => true
      case _            => false

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = that match
      case Zero         => true
      case Successor(m) => this.predecessor > m

    @targetName("addition")
    infix def +(that: Numeral): Numeral = new Successor(n + that)

    // Optional challenge.
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = that match
      case Zero         => this
      case Successor(m) => this.predecessor - m

    override def toString: String = s"Nat($predecessor)"

    override def equals(obj: Any): Boolean = obj match
      case s: Successor => this.predecessor == s.predecessor
      case _            => false
