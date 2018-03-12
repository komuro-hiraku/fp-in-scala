package chapter4

sealed trait Validated[+E, +A] {

  def map[B](f: A => B): Validated[E, B] =
//    Validated.map2(this, Valid())((a, _) => f(a))
    this match {
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)
    }

  def orElse[EE >: E, B >: A](ob: Validated[EE, B]): Validated[EE, B] = (this, ob) match {
    case (Valid(a), _) => Valid(a)
    case (_, Valid(_)) => ob
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
  }
}

case class Invalid[+E](errors: List[E]) extends Validated[E, Nothing]
case class Valid[+A](value: A) extends Validated[Nothing, A]


object Validated {

  /**
    * ２つのValidatedな値を組み合わせる
    * @param a １つ目のValidated
    * @param b ２つ目のValidated
    * @param f (A, B) => C な関数
    * @tparam E1 １つ目のValidatedが出力する可能性のあるError型
    * @tparam E2 ２つ目のValidatedが出力する可能性のあるError型
    * @tparam A １つ目のValidatedの有効値の型
    * @tparam B ２つ目のValidatedの有効値の型
    * @tparam C 関数的用後の結果値の型
    * @return Validated[E2, C]
    */
  def map2[E1, E2 >: E1, A, B, C](a: Validated[E1, A], b: Validated[E2, B])(f:(A, B) => C): Validated[E2, C] = (a, b) match {
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (Valid(aa), Valid(bb)) => Valid(f(aa, bb))
    case (Invalid(e1), _) => Invalid(e1)
    case (_, Invalid(e2)) => Invalid(e2)
  }
}