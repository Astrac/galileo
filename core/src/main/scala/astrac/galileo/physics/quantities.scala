package astrac.galileo.physics

import cats.Monoid
import io.estatico.newtype.Coercible
import io.estatico.newtype.macros.newtype
import spire.algebra.{Field, InnerProductSpace}
import spire.syntax.field._

case class Vector[A](x: A, y: A)

object Vector {
  private def makeVectorSpaceInstance[A](
      aField: Field[A]
  ): InnerProductSpace[Vector[A], A] =
    new InnerProductSpace[Vector[A], A] {
      implicit def scalar: Field[A] = aField

      def dot(a: Vector[A], b: Vector[A]): A = (a.x * b.x) + (a.y * b.y)

      def negate(x: Vector[A]): Vector[A] =
        Vector(scalar.negate(x.x), scalar.negate(x.y))

      def plus(x: Vector[A], y: Vector[A]): Vector[A] =
        Vector(x.x + y.x, x.y + y.y)

      def timesl(r: A, v: Vector[A]): Vector[A] =
        Vector(v.x * r, v.y * r)

      def zero: Vector[A] =
        Vector(scalar.zero, scalar.zero)
    }

  implicit def vectorSpaceInstance[A: Field]: InnerProductSpace[Vector[A], A] =
    makeVectorSpaceInstance(Field[A])
}

object quantities {
  @newtype case class Position[A](vector: Vector[A])
  @newtype case class Velocity[A](vector: Vector[A])
  @newtype case class Acceleration[A](vector: Vector[A])
  @newtype case class Force[A](vector: Vector[A])
  @newtype case class Time[A](value: A)
  @newtype case class Mass[A](value: A)

  implicit def vectorSpaceMonoid[A](
      implicit vs: InnerProductSpace[A, _]
  ): Monoid[A] =
    new Monoid[A] {
      def empty = vs.zero
      def combine(a: A, b: A): A = vs.plus(a, b)
    }

  implicit def coercibleField[A, B](
      implicit ev: Coercible[Field[B], Field[A]],
      fb: Field[B]
  ): Field[A] =
    ev(fb)

  implicit def coercibleVectorSpace[A, B](
      implicit toB: Coercible[Vector[A], B],
      toVector: Coercible[B, Vector[A]],
      vs: InnerProductSpace[Vector[A], A]
  ): InnerProductSpace[B, A] =
    new InnerProductSpace[B, A] {
      implicit def scalar: Field[A] = vs.scalar
      def dot(a: B, b: B): A = vs.dot(toVector(a), toVector(b))
      def negate(x: B): B = toB(vs.negate(toVector(x)))
      def plus(x: B, y: B): B = toB(vs.plus(toVector(x), toVector(y)))
      def timesl(r: A, v: B): B = toB(vs.timesl(r, toVector(v)))
      def zero: B = toB(vs.zero)
    }
}
