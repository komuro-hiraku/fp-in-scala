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

  "intDouble" should {

    /**
      * なんかめっちゃ怒られとる
      * Warning:(24, 54) Type parameter should not be specified because it will be erased at runtime, please use _ instead.
      * Note that in future version of ScalaTest this will give a compiler error.
      * RNG.intDouble(Simple(100L))._1 mustBe a[Tuple2[Int, Double]]
      */
    "Int, Doubleのランダム値のタプルを返す" in {
      RNG.intDouble(Simple(100L))._1 mustBe a[(Int, Double)]
    }

    "新たなRNGオブジェクトを返す" in {
      val rng = Simple(100L)
      RNG.intDouble(rng)._2 mustNot equal(rng)
    }

    "Int, Doubleの値は異なる" in {
      val result = RNG.intDouble(Simple(100L))._1
      result._1 mustNot equal(result._2.toInt)
    }
  }

  "ints" should {

    "4個の値はすべて異なる" in {
      val randomList = RNG.ints(4)(Simple(100L))._1
      randomList mustBe List(1438213179, 288555289, 374484099, 419891633, 38474890)
    }

    "返却されたRngは異なる" in {
      val rng = Simple(102L)
      RNG.ints(15)(rng)._2 mustNot equal(rng)
    }
  }
}
