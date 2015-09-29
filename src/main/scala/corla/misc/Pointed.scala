package corla.misc

import scalaz._

trait Pointed[F[_]] {
  def point[A](a: A): F[A]
}

object Pointed {
  def apply[F[_]:Pointed]: Pointed[F] = implicitly

  implicit val identityPointed = new Pointed[Id.Id] {
    def point[A](a: A): Id.Id[A] = a
  }

  implicit def applicativePointed[F[_]](implicit F: Applicative[F]) = new Pointed[F] {
    def point[A](a: A): F[A] = F.point(a)
  }
}
