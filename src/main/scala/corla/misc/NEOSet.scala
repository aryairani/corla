package corla.misc

import scalaz.Ordering._
import scalaz._

/**
 * Created by arya on 12/5/14.
 */

object NEOSet {
  type NEOSet[A] = OneAnd[ISet,A]
  def apply[A:Order](head: A, tail: ISet[A]): NEOSet[A] = OneAnd[ISet,A](head, tail delete head)
  def apply[A:Order](head: A, tail: A*): NEOSet[A] = OneAnd[ISet,A](head, ISet.fromList(tail.toList) delete head)

  implicit class NonEmptyOrderedSetSyntax[A](s: NEOSet[A])(implicit A:Order[A]) {
    import scalaz.syntax.foldable._

    def head = s.head
    def tail = s.tail
    def toISet: ISet[A] = tail insert head
    def toSet: Set[A] = tail.toSet + head
    def size = s.tail.size + 1

    def insert(a: A): NEOSet[A] = NEOSet(head, tail insert a delete head)
    def delete(a: A): ISet[A] = toISet delete a

    def map[B:Order](f: A => B) = {
      val fhead: B = f(head)
      NEOSet(fhead, tail map f delete fhead)
    }

    def flatMap[B:Order](f: A => NEOSet[B]): NEOSet[B] = {
      val fhead = f(head)
      val ftails = tail map f
      NEOSet(fhead.head, fhead.tail union ftails.map(_.toISet).suml delete fhead.head)
    }

    def contains(x: A): Boolean = A.equal(x,head) || tail.contains(x)
  }

  def argmaxesBy[F[_],A:Order,B](fa: F[A])(f: A => B)(implicit F: Foldable1[F], ord: math.Ordering[B]): NEOSet[A] =
    F.foldMapLeft1[A,(NEOSet[A],Option[B])](fa)(a => (NEOSet(a),Some(f(a)))) {
      case (maxes @ (as, bs @ Some(b0)), a) =>
        val b = f(a)
        fromInt(ord.compare(b,b0)) match {
          case GT => // a > max
            NEOSet(a) -> Some(b)
          case EQ => // a = max
            (as insert a, bs)
          case LT => // a < max
            maxes
        }
    } ._1

}
