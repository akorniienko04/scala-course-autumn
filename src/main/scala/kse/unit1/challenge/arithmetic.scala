package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val Z: Number => Number = value => if value == 0 then 1 else 0
  val S: Number => Number = value => value + 1

  /**
   * Optional task: make `addition` tail-recursive.
   */
  val addition: (Number, Number) => Number =
    (l, r) =>

      @tailrec
      def loop(left: Number, right: Number): Number =
        if right == 0 then left
        else loop(S(left), right - 1)

      loop(l, r)

  /**
   * Optional task: make `multiplication` tail-recursive.
   */
  val multiplication: (Number, Number) => Number =
    (l, r) =>

      @tailrec
      def loop(acc: Number, times: Number): Number =
        if times == 0 then acc
        else loop(addition(acc, l), times - 1)

      loop(0, r)

  /**
   * Optional task: make `power` tail-recursive.
   */
  val power: (Number, Number) => Number =
    (osnova, p) =>

      @tailrec
      def loop(res: Number, exp: Number): Number =
        if exp == 0 then res
        else loop(multiplication(res, osnova), exp - 1)

      loop(1, p)
