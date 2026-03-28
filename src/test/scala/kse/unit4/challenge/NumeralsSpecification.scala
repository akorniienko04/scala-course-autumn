package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero is zero") = forAll { (z: Zero) => z.isZero }

  property("Successor is not zero") = forAll { (s: Successor) =>
    !s.isZero
  }

  property("successor of any numeral is not zero") = forAll { (n: Numeral) =>
    !n.successor.isZero
  }

  property("pred of successor is the original numeral") = forAll { (n: Numeral) =>
    n.successor.predecessor == n
  }

  property("Zero is not greater than any numeral") = forAll { (n: Numeral) =>
    !(Zero > n)
  }

  property("Successor is greater than Zero") = forAll { (s: Successor) =>
    s > Zero
  }

  property("no numeral is greater than itself") = forAll { (n: Numeral) =>
    !(n > n)
  }

  property("less than is the opposite of greater or equal") = forAll { (a: Numeral, b: Numeral) =>
    (a < b) == !(a >= b)
  }

  property("less or equal is the opposite of greater than") = forAll { (a: Numeral, b: Numeral) =>
    (a <= b) == !(a > b)
  }

  property("addition with Zero on the right") = forAll { (n: Numeral) =>
    (n + Zero) == n
  }

  property("addition with Zero on the left") = forAll { (n: Numeral) =>
    (Zero + n) == n
  }

  property("greater or equal means greater or equal") = forAll { (a: Numeral, b: Numeral) =>
    (a >= b) == ((a > b) || (a == b))
  }

  property("subtracting Zero changes nothing") = forAll { (n: Numeral) =>
    (n - Zero) == n
  }

  property("subtracting a numeral from itself gives Zero") = forAll { (n: Numeral) =>
    (n - n) == Zero
  }

  property("subtraction saturates at Zero") = forAll { (n: Numeral) =>
    (Zero - n) == Zero
  }

  property("equality is reflexive") = forAll { (n: Numeral) =>
    n == n
  }

  property("equality is symmetric") = forAll { (a: Numeral, b: Numeral) =>
    (a == b) == (b == a)
  }

end NumeralsSpecification
