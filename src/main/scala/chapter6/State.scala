package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Rand[+A] っていう型は RNG => (A, RNG) を返す関数にしろってこと？
  type Rand[+A] = RNG => (A, RNG)

  // Rand[Int] なので RNG => (Int, RNG)？
//  def randomInt(rnd: RNG):(Int, RNG) = rnd.nextInt
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  /** Exercise 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) (i + 1) * (-1) else i, r)
    // Integer.MIN_VALUEをそのまま符号ひっくり返すとマズイ気がするので一応 +1
  }

  /** Exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Math.nextUp(Int.MaxValue.toDouble), r) // iの範囲は 0 < Integer.MAX_VALUEまで
  }

  /** Exercise 6.3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, ir) = nonNegativeInt(rng)
    val (d, dr) = double(ir)
    ((i, d), dr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, dr) = double(rng)
    val (i, ir) = nonNegativeInt(dr)
    ((d, i), ir)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = ???

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input