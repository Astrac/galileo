package astrac.galileo.physics

import cats.Monoid
import io.estatico.newtype.Coercible
import io.estatico.newtype.macros.newsubtype
import spire.algebra.{Field, InnerProductSpace}

object Quantities {
  @newsubtype case class Position[A](vector: Vector[A])
  @newsubtype case class Velocity[A](vector: Vector[A])
  @newsubtype case class Acceleration[A](vector: Vector[A])
  @newsubtype case class Force[A](vector: Vector[A])
  @newsubtype case class Time[A](value: A)
  @newsubtype case class Mass[A](value: A)
  @newsubtype case class Length[A](value: A)
  @newsubtype case class Stiffness[A](value: A)
  @newsubtype case class Damping[A](value: A)

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

  implicit def coercibleVectorSpace2[A, B, C](
      implicit toB: Coercible[Vector[C], B],
      toVector: Coercible[B, Vector[C]],
      toA: Coercible[C, A],
      toC: Coercible[A, C],
      toFieldA: Coercible[Field[C], Field[A]],
      vs: InnerProductSpace[Vector[C], C]
  ): InnerProductSpace[B, A] =
    new InnerProductSpace[B, A] {
      implicit def scalar: Field[A] = toFieldA(vs.scalar)
      def dot(a: B, b: B): A = toA(vs.dot(toVector(a), toVector(b)))
      def negate(x: B): B = toB(vs.negate(toVector(x)))
      def plus(x: B, y: B): B = toB(vs.plus(toVector(x), toVector(y)))
      def timesl(r: A, v: B): B = toB(vs.timesl(toC(r), toVector(v)))
      def zero: B = toB(vs.zero)
    }
}

trait Quantities[A] {
  type Length = Quantities.Length[A]
  def Length(a: A) = Quantities.Length(a)

  type Stiffness = Quantities.Stiffness[A]
  def Stiffness(a: A) = Quantities.Stiffness(a)

  type Damping = Quantities.Damping[A]
  def Damping(a: A) = Quantities.Damping(a)

  type Position = Quantities.Position[A]
  def Position(a: A, b: A) = Quantities.Position(Vector(a, b))

  type Velocity = Quantities.Velocity[A]
  def Velocity(a: A, b: A) = Quantities.Velocity(Vector(a, b))

  type Acceleration = Quantities.Acceleration[A]
  def Acceleration(a: A, b: A) = Quantities.Acceleration(Vector(a, b))

  type Force = Quantities.Force[A]
  def Force(a: A, b: A) = Quantities.Force(Vector(a, b))

  type Mass = Quantities.Mass[A]
  def Mass(a: A) = Quantities.Mass(a)

  type Time = Quantities.Time[A]
  def Time(a: A) = Quantities.Time(a)

  type Spring = Rigid.Spring[A]
  def Spring(l: Length, s: Stiffness, d: Damping) = Rigid.Spring(l, s, d)

}
