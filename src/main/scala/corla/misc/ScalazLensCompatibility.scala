package corla.misc

import monocle.PLens

import scalaz._

trait ScalazLensCompatibility {
  implicit class MonocleScalazLensSyntax[S,T,A,B](l: monocle.PLens[S,T,A,B]) {

    def xmapA[X1, X2](f: T => X2)(g: X1 => S): monocle.PLens[X1, X2, A, B] =
      PLens[X1,X2,A,B](p => l.get(g(p)))(b => p => f(l.set(b)(g(p))))

    def xmapbA[X, Z >: T <: S](b: scalaz.BijectionT.Bijection[Z, X]): monocle.PLens[X, X, A, B] =
      xmapA(b to _)(b from _)

    def xmapB[X1, X2](f: A => X1)(g: X2 => B): monocle.PLens[S, T, X1, X2] =
      PLens[S,T,X1,X2](s => f(l.get(s)))(x2 => s => l.set(g(x2))(s))

    def xmapbB[X, Z >: A <: B](b: scalaz.BijectionT.Bijection[Z, X]): monocle.PLens[S, T, X, X] =
      xmapB(b to _)(b from _)

    def get(s: S): A =
      l.get(s)

    def set(s: S, b: B): T =
      l.set(b)(s)

    def st: State[S, A] =
      State(s => (s, l.get(s)))

    /** Modify the value viewed through the lens */
    def mod(f: A => B, s: S): T =
      l.modify(f)(s)

    def =>=(f: A => B): S => T =
      mod(f, _)

    /** Modify the value viewed through the lens, returning a functor `X` full of results. */
    def modf[F[_]](f: A => F[B], s: S)(implicit F: Functor[F]): F[T] =
      l.modifyF(f)(s)

    def =>>=[F[_]](f: A => F[B])(implicit F: Functor[F]): S => F[T] =
      modf(f, _)

    /** Modify the value viewed through the lens, returning a `C` on the side.  */
    def modp[C](f: A => (B, C), s: S): (T, C) = {
      val (b, c) = f(l.get(s))
      (l.set(b)(s), c)
    }

    /** Modify the portion of the state viewed through the lens and return its new value. */
    def mods(f: A => B): IndexedState[S, T, B] =
      IndexedState[S,T,B]{ s =>
        val a = l.get(s)
        val b = f(a)
        l.set(b)(s) -> b
      }

    /** Modify the portion of the state viewed through the lens and return its new value. */
    def %=(f: A => B): IndexedState[S, T, B] =
      mods(f)

    /** Modify the portion of the state viewed through the lens and return its old value.
      * @since 7.0.2
      */
    def modo(f: A => B): IndexedState[S, T, A] =
      IndexedState[S,T,A](s => {
        val a: A = l.get(s)
        (l.set(f(a))(s), a)
      })

    /** Modify the portion of the state viewed through the lens and return its old value. alias for `modo`
      * @since 7.0.2
      */
    def <%=(f: A => B): IndexedState[S, T, A] =
      modo(f)

    /** Set the portion of the state viewed through the lens and return its new value. */
    def assign(b: => B): IndexedState[S, T, B] =
      mods(_ => b)

    /** Set the portion of the state viewed through the lens and return its new value. */
    def :=(b: => B): IndexedState[S, T, B] =
      assign(b)

    /** Set the portion of the state viewed through the lens and return its old value.
      * @since 7.0.2
      */
    def assigno(b: => B): IndexedState[S, T, A] =
      modo(_ => b)

    /** Set the portion of the state viewed through the lens and return its old value. alias for `assigno`
      * @since 7.0.2
      */
    def <:=(b: => B): IndexedState[S, T, A] =
      assigno(b)

    /** Modify the portion of the state viewed through the lens, but do not return its new value. */
    def mods_(f: A => B): IndexedState[S, T, Unit] =
      IndexedState(s => (mod(f, s), ()))

    /** Modify the portion of the state viewed through the lens, but do not return its new value. */
    def %==(f: A => B): IndexedState[S, T, Unit] =
      mods_(f)

    /** Contravariantly map a state action through a lens. */
    def lifts[C](s: IndexedState[A, B, C]): IndexedState[S, T, C] =
      IndexedState(a => modp(s(_), a))

    def %%=[C](s: IndexedState[A, B, C]): IndexedState[S, T, C] =
      lifts(s)

    /** Map the function `f` over the lens as a state action. */
    def map[C](f: A => C): State[S, C] =
      State(a => (a, f( get(a))))

    /** Map the function `f` over the value under the lens, as a state action. */
    def >-[C](f: A => C): State[S, C] = map(f)

    /** Bind the function `f` over the value under the lens, as a state action. */
    def flatMap[C](f: A => IndexedState[S, T, C]): IndexedState[S, T, C] =
      IndexedState(a => f(get(a))(a))

    /** Bind the function `f` over the value under the lens, as a state action. */
    def >>-[C](f: A => IndexedState[S, T, C]): IndexedState[S, T, C] =
      flatMap[C](f)

    /** Sequence the monadic action of looking through the lens to occur before the state action `f`. */
    def ->>-[C](f: => IndexedState[S, T, C]): IndexedState[S, T, C] =
      flatMap(_ => f)

    /** Contravariantly mapping the state of a state monad through a lens is a natural transformation */
    def liftsNT: ({type m[x] = IndexedState[A,B,x]})#m ~> ({type n[x] = IndexedState[S,T,x]})#n =
      new (({type m[x] = IndexedState[A,B,x]})#m ~> ({type n[x] = IndexedState[S,T,x]})#n) {
        def apply[C](s : IndexedState[A,B,C]): IndexedState[S,T,C] = IndexedState[S,T,C](a => modp(s(_), a))
      }
  }
}

object ScalazLensCompatibility extends ScalazLensCompatibility
