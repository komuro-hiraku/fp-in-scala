package chapter5

import org.scalatest.{FlatSpec, MustMatchers, WordSpec}

class StreamSpec extends WordSpec with MustMatchers {

  "toList" should {
    "StreamをListに変換できる" in {
      Stream(1, 2, 3).toList mustBe List(1, 2, 3)
    }

    "空Streamの場合は空Listが返却される" in {
      Stream.empty.toList mustBe List.empty
    }
  }

  "take" should {

    "5つの要素からなるStream先頭の3つを取り出せる" in {
      Stream(5, 4, 3, 2, 1).take(3).toList mustBe List(5, 4, 3)
    }

    "0を指定すると空Streamが返却される" in {
      Stream(5, 4, 3, 2, 1).take(0) mustBe Stream.empty
    }

    "空Streamでは空Streamが返却される" in {
      Stream.empty.take(5) mustBe Stream.empty
    }
  }

  "drop" should {

    "5つの要素からなるStream先頭の3つをSkipする" in {
      Stream(5, 4, 3, 2, 1).drop(3).toList mustBe List(2, 1)
    }

    "0を指定するとStream自身が返却される" in {
      Stream(5, 4, 3, 2, 1).drop(0).toList mustBe List(5, 4, 3, 2, 1)
    }

    "空Streamでは空Streamが返却される" in {
      Stream.empty.drop(5) mustBe Stream.empty
    }
  }

  "takeWhile" should {

//    "3で割り切れるものだけ出力" in {
//      Stream(1,3,5,7,9,11,16,21).takeWhile(p => p % 3 == 0) must be equals Stream(3, 9, 21)
//    }

    "3以上のものだけ出力" in {
      Stream(5, 4, 3, 2, 1, 3, 4, 5).takeWhile(_ >= 3).toList mustBe List(5, 4, 3)
    }

    "1つも当てはまらない場合は空Stream" in {
      Stream(1, 3, 5, 7, 9).takeWhile(_ > 2) mustBe Stream.empty
    }

    "空Streamでは空Streamが返却される" in {
      val f = (p:Int) => p > 2
      Stream.empty.takeWhile(f) mustBe Stream.empty
    }
  }

  "flatMap" should {

    "Stream(1, 2, 3)をDouble型で3倍したStreamを出力する" in {
      Stream(1, 2, 3).flatMap(s => Stream(s * 3.0)).toList mustBe List(3.0, 6.0, 9.0)
    }
  }

  "constance" should {

    "無限Streamの中から5つ要素を取り出す" in {
      Stream.constance("A").take(5).toList mustBe List("A", "A", "A", "A", "A")
    }
  }

  "from" should {

    "公差1の等差数列で5まで出力する" in {
      Stream.from(1).take(5).toList mustBe List(1, 2, 3, 4, 5)
    }

    "公差1の等差数列で10まで出力する" in {
      Stream.from(1).take(10).toList mustBe List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    }
  }

  "fibs" should {

    "Fibonacci数列を7個まで出力する" in {
      Stream.fibs().take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }
  }

  "unfold" should {

    "5倍を重ね続けた値を3つ取る" in {
      Stream.unfold(1)(s => Some((s, s * 5))).take(3).toList mustBe List(1, 5, 25)
    }

  }

  "unfoldOnes" should {

    "5個の1が出力される" in {
      Stream.unfoldOnes.take(5).toList mustBe List(1, 1, 1, 1, 1)
    }
  }

  "unfoldFrom" should {

    "交差1の等差数列で5まで出力する" in {
      Stream.unfoldFrom(1).take(5).toList mustBe List(1, 2, 3, 4, 5)
    }

  }

}
