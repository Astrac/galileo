package astrac.galileo.physics

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
