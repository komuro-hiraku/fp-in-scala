package chapter4


sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)
//    this match {
//      case Some(a) => f(a)
//      case None => None
//    }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map((a) => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap((a) => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /**
    * 平均値を算出する
    * @param xs Double型のSequence
    * @return Option[Double]
    */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((m) => {
      val sum = xs.foldRight(0.0)((x, acc) => acc + math.pow(x - m, 2))
      Some(sum / xs.length)
    })  // 後半meanとやってること一緒やんけ！！！
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aa), Some(bb)) => Some(f(aa, bb))
    case _ => None
  }
//    for {
//      aa <- a
//      bb <- b
//    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight[Option[List[A]]](Some(List.empty))((a, acc) => map2(a, acc)((x, y) => x :: y ))
    a match {
      case Nil => None
      case head :: tail => head.flatMap(head2 => sequence(tail).map(rest => head2 :: rest))
    }

  /**
    * 問題文の意味がよくわからなかったTraverse
    * @param a 処理したいList
    * @param f A => Option[B] の関数
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, acc)  // 右から畳み込み演算
      => map2(f(a), acc)((h, t) => h :: t)            // 取得した要素にfを適用してAccumulatorと共にListで繋ぐ
    )
}
