package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean =
    case True  => False
    case False => True


  val conjunction: (Boolean, => Boolean) => Boolean = (x, y) =>
    x match
      case False => False
      case True =>
        y match
          case True  => True
          case False => False

  val disjunction: (Boolean, => Boolean) => Boolean = (x, y) =>
    x match
      case True => True
      case False =>
        y match
          case False => False
          case True  => True

  val implication: (Boolean, => Boolean) => Boolean = (x, y) =>
    x match
      case False => True
      case True =>
        y match
          case True  => True
          case False => False

  val equivalence: (Boolean, => Boolean) => Boolean = (x, y) =>
    x match
      case True =>
        y match
          case True => True
          case False => False
      case False =>
        y match
          case True => False
          case False => True

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =
    @tailrec
    def foldReq(list: List[Boolean], acc: Boolean): Boolean =
      list match
        case Nil => acc
        case head :: tail => foldReq(tail, operation(acc, head))

    foldReq(list, unit)

  val conjunctionOfElements: List[Boolean] => Boolean = fold(operation = conjunction, unit = True)
  val disjunctionOfElements: List[Boolean] => Boolean = fold(operation = disjunction, unit = False)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)
