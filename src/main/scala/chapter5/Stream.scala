package chapter5

import Stream._

trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /** Exercise 5.1 */
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  /** Exercise 5.2 */

  // 先頭からn個取り出す
  def take(n: Int): Stream[A] = {
    def loop(as: Stream[A], count: Int): Stream[A] = as match {
      case Cons(h ,t) if count > 0 => cons(h(), loop(t(), count - 1))
      case _ => empty
    }
    loop(this, n)
  }

  // 模範解答
//  def take(n: Int): Stream[A] = this match {
//    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
//    case Cons(h, _) if n == 1 => cons(h(), empty)
//    case _ => empty
//  }

  // 先頭からn個スキップする
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop( n - 1)
    case Cons(_, t) if n == 1 => t()
    case Cons(_, _) if n == 0 => this
    case _ => empty
  }

  /** Exercise5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
//    case Cons(_, t) => t().takeWhile(p)
    case _ => Stream.empty
  }

  /** 5.3 exists function */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p) // 第二引数は必要になるまで評価しない
    case _ => false
  }

  /** fの第二引数は名前渡しで受け取ることと、
    * 評価しないという選択が可能であることを意味する？　とは？ */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** fの第二引数。つまりbを評価しない可能性がある。このfoldRightは途中で走査を打ち切る */
  def exists_(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /** 全部trueならtrueで一個でもfalseならfalseって理解でOK？ */
  def forAll(p:A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /** p(a) = falseの時点でemptyを返す. 後続の処理は続かない */
  def foldRightTakeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)

  /** compile通るまで試行錯誤しただけ感はある */
  def foldRightHeadOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  /** Exercise 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f:A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

}

case object Empty extends Stream[Nothing]

// コンストラクタ(非正格)
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // スマートコンストラクタ
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd  // 遅延評価
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /** Exercise5.8 無限Streamを返す */
  def constance[A](a: A): Stream[A] =
    Stream.cons(a, constance(a))

  /** Exercise5.9 n, n+1, n+2と続く無限Streamを生成する */
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  /** Exercise5.10 fibsの超力技 */
  def fibs(): Stream[Int] = {
//    def loop(n1:Int, n2:Int, acc:Stream[Int]): Stream[Int] = (n1, n2) match {
//      case (0, 1) => Stream.cons(n1, Stream.cons(n2, Stream.cons(n2 + n1, loop(n2, n1 + n2, acc))))
//      case _ => Stream.cons(n1 + n2, loop(n2, n1 + n2 ,acc))
//    }
//    loop(0, 1, empty)
    def loop(n1: Int, n2: Int): Stream[Int] =
      Stream.cons(n1, loop(n2, n1 + n2))
    loop(0, 1)
  }

  /** Exercise5.11 unfold
    * 初期状態に加えて以下の状態と生成されるストリームの次の値を生成する関数を受け取る
    * 何言ってんだお前？
    * */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  def unfoldOnes = unfold(1)(s => Some(1, 1))

  /** 5.12 */
  def unfoldFibs(): Stream[Int] = {
//    unfold((0, 1)){case (s0, s1) => Some(s0, (s1, s0 + s1))}
    unfold((0, 1))(pn => Some(pn._1, (pn._2, pn._1 + pn._2)))
  }

  def unfoldFrom(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}

