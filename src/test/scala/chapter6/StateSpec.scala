package chapter6

import org.scalatest.{MustMatchers, WordSpec}

class StateSpec extends WordSpec with MustMatchers {

  import RNG._


  "RNG" should {

    "NonNegativeは0からInteger.MAX_VALUEの間で正の値を持つ" in {
      RNG.nonNegativeInt(Simple(100L))._1 must (be >= 0 and be <= Int.MaxValue)
    }

    "Doubleのタプルは0.0から1.0の間でDoubleの値を持つ" in {
      RNG.double(Simple(100L))._1 must (be >= 0.0 and be < 1.0)
    }
  }
}
