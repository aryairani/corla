package corla.memory

import scalaz.Traverse

case class PrimitiveMemory(isFirstAction: Boolean) extends AnyVal
object PrimitiveMemory {
  implicit def primitiveMemory2[S,A]: EmptyMemory2[PrimitiveMemory,S,A] = new EmptyMemory2[PrimitiveMemory, S,A] {
    def empty = PrimitiveMemory(isFirstAction = true)
    def addExperience = _ => _ => PrimitiveMemory(isFirstAction = false)
    def addBatchExperience[F[_] : Traverse] = _ => _ => PrimitiveMemory(isFirstAction = false)
  }

  implicit def memory3[S,A] =
    new EmptyMemory3[PrimitiveMemory,S,A] with NativeBatch3[PrimitiveMemory,S,A] {
      def empty: PrimitiveMemory = PrimitiveMemory(isFirstAction = true)
      def addBatchExperience[F[_] : Traverse] = _ => _ => PrimitiveMemory(isFirstAction = false)
    }
}
