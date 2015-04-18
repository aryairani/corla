package corla.misc

/**
 * Created by arya on 12/8/14.
 */

import scalaz.Ordering._
import scalaz._

object NESet {
  type NESet[A] = OneAnd[Set,A]
  def apply[A](head: A, tail: Set[A]): NESet[A] = OneAnd[Set,A](head, tail - head)
  def apply[A](head: A, tail: A*): NESet[A] = OneAnd[Set,A](head, tail.toSet - head)

  implicit class NESetSyntax[A](s: NESet[A]) {
    import scalaz.syntax.foldable._

    def head = s.head
    def tail = s.tail
    //    def toISet: ISet[A] = tail + head
    def toSet: Set[A] = tail.toSet + head
    def toList: List[A] = head :: tail.toList
    def toNEL: NonEmptyList[A] = NonEmptyList(head, tail.toSeq: _*)
    def size = s.tail.size + 1

    def insert(a: A): NESet[A] = NESet(head, tail + a -head)
    def delete(a: A): Set[A] = toSet - a

    def map[B](f: A => B) = {
      val fhead: B = f(head)
      NESet(fhead, (tail map f) - fhead)
    }

    def flatMap[B](f: A => NESet[B]): NESet[B] = {
      val fhead = f(head)
      val ftails = tail map f
      import scalaz.std.set._
      NESet(fhead.head, fhead.tail union ftails.map(_.toSet).suml - fhead.head)
    }

    def contains(x: A): Boolean = (x == head) || tail.contains(x)

    def toMap[K,V](implicit ev: A <:< (K,V)) = toSet.toMap

    def filter(p: A => Boolean): Set[A] = toSet.filter(p)
  }

  def argmaxesBy[F[_],A,B](fa: F[A])(f: A => B)(implicit F: Foldable1[F], ord: math.Ordering[B]): NESet[A] =
    F.foldMapLeft1[A,(NESet[A],Option[B])](fa)(a => (NESet(a),Some(f(a)))) {
      case (maxes @ (as, bs @ Some(b0)), a) =>
        val b = f(a)
        fromInt(ord.compare(b,b0)) match {
          case GT => // a > max
            NESet(a) -> Some(b)
          case EQ => // a = max
            (as insert a, bs)
          case LT => // a < max
            maxes
        }
    } ._1

}
