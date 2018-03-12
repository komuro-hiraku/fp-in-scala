package chapter4

import org.scalatest.{FlatSpec, MustMatchers}

class OptionSpec extends FlatSpec with MustMatchers {

  "map" should "値が存在すれば関数を適用できる" in {
    Some(3).map(_ * 12) mustBe Some(36)
  }

  "map" should "値が存在しなければ関数は適用されない" in {
    None.map(_.toString) mustBe None
  }

  "flatMap" should "値が存在すれば中身を取り出して関数を適用する" in {
    Some(123).flatMap((a: Int) => Some(a / 3.0)) mustBe Some(41.0)
  }

  "flatMap" should "値が存在しなければ関数を適用されない" in {
    val f = (a:Int) => Some(a / 3.0)
    None.flatMap(f) mustBe None
  }

  "getOrElse" should "値が存在すれば中身を取り出して返す" in {
    Some("I am a pen").getOrElse("Default Value") mustBe "I am a pen"
  }

  "getOrElse" should "値が存在しなければデフォルト値を返す" in {
    None.getOrElse("Default Value") mustBe "Default Value"
  }

  "filter" should "評価式が真であれば出力される" in {
    Some(4).filter((a: Int) => a % 2 == 0) mustBe Some(4)
  }

  "filter" should "評価式が偽であれば出力されない" in {
    Some("I have a pen").filter((a: String) => a.contains("ballpen")) mustBe None
  }

  "mean" should "平均が計算される" in {
    Option.mean(List(3, 4, 5)) mustBe Some(4)
  }

  "mean" should "ListがゼロならばNoneが出力される" in {
    Option.mean(List.empty) mustBe None
  }

  "map2" should "双方ともに値が存在すれば関数が適用される" in {
    Option.map2(Some(3), Some(4))((a, b) => a + b) mustBe Some(7)
  }

  "map2" should "bに値が存在しなければ関数は適用されずにNoneが出力される" in {
    Option.map2(Some(3), None)((a, b) => a + b) mustBe None
  }

  "map2" should "aに値が存在しなければ関数は適用されずにNoneが出力される" in {
    val f = (a: Int, b: Int) => a + b
    Option.map2(None, Some(4))(f) mustBe None
  }

  "map2" should "a, bいずれも値が存在しなければ関数は適用されずにNoneが出力される" in {
    val f = (a: Int, b: Int) => a + b
    Option.map2(None, None)(f) mustBe None
  }
}