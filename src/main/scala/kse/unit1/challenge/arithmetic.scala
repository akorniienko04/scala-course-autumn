package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val Z: Number => Number = _ => 0

  val S: Number => Number = value => value + 1

  /**
   * Optional task: make `addition` tail-recursive.
   */
  val addition: (Number, Number) => Number =
    (l, r) =>

      require(l >= 0, "Left must be non negative")
      require(r >= 0, "Right must be non negative")

      @tailrec
      def loop(left: Number, right: Number): Number =
        right match
          case 0 => left
          case _ => loop(S(left), right - 1)

      loop(l, r)

  /**
   * Optional task: make `multiplication` tail-recursive.
   */
  val multiplication: (Number, Number) => Number =
    (l, r) =>

      require(l >= 0, "Left must be non negative")
      require(r >= 0, "Right must be non negative")

      @tailrec
      def loop(acc: Number, times: Number): Number =
        times match
          case 0 => acc
          case _ => loop(addition(acc, l), times - 1)

      loop(Z(0), r)

  /**
   * Optional task: make `power` tail-recursive.
   */
  val power: (Number, Number) => Number =
    (osnova, p) =>

      require(p >= 0, "Power must be non negative")
      require(osnova != 0 || p != 0)

      @tailrec
      def loop(res: Number, exp: Number): Number =
        exp match
          case 0 => res
          case _ => loop(multiplication(res, osnova), exp - 1)

      loop(S(Z(0)), p)
