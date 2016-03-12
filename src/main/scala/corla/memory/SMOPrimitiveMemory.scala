package corla.memory

import scalaz.Traverse

case class SMOPrimitiveMemory(hasActed: Boolean) extends AnyVal
object SMOPrimitiveMemory {
  implicit def primitiveMemory2[S,A]: EmptyMemory2[SMOPrimitiveMemory,S,A] = new EmptyMemory2[SMOPrimitiveMemory, S,A] {
    def empty = SMOPrimitiveMemory(hasActed = false)
    def addExperience = _ => _ => SMOPrimitiveMemory(hasActed = true)
    def addBatchExperience[F[_] : Traverse] = _ => _ => SMOPrimitiveMemory(hasActed = true)
  }

  implicit def memory3[S,A] =
    new EmptyMemory3[SMOPrimitiveMemory,S,A] with NativeBatch3[SMOPrimitiveMemory,S,A] {
      def empty: SMOPrimitiveMemory = SMOPrimitiveMemory(hasActed = false)
      def addBatchExperience[F[_] : Traverse] = _ => _ => SMOPrimitiveMemory(hasActed = true)
    }
}
