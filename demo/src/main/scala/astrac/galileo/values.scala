package astrac.galileo.demo

import cats.Monoid
import cats.syntax.semigroup._

case class Vec2(x: Double, y: Double) {
  def modulo = math.sqrt(x * x + y * y)
  def unit = Vec2(x / modulo, y / modulo)
  def scale(f: Double) = Vec2(x * f, y * f)
  def -(other: Vec2) = Vec2(x - other.x, y - other.y)
  def *(other: Vec2) = Vec2(x * other.x, y * other.y)
  def /(value: Double) = Vec2(x / value, y / value)
}
object Vec2 {
  implicit val monoidInstance = new Monoid[Vec2] {
    lazy val empty = Vec2(0, 0)
    def combine(a: Vec2, b: Vec2): Vec2 = Vec2(a.x + b.x, a.y + b.y)
  }
}

case class Velocity(vec: Vec2)
object Velocity {
  implicit val monoidInstance = new Monoid[Velocity] {
    lazy val empty = Velocity(Monoid[Vec2].empty)
    def combine(a: Velocity, b: Velocity): Velocity =
      Velocity(a.vec |+| b.vec)
  }
}

case class Acceleration(vec: Vec2)
object Acceleration {
  implicit val monoidInstance = new Monoid[Acceleration] {
    lazy val empty = Acceleration(Monoid[Vec2].empty)
    def combine(a: Acceleration, b: Acceleration): Acceleration =
      Acceleration(a.vec |+| b.vec)
  }
}

case class Position(vec: Vec2)
object Position {
  implicit val monoidInstance = new Monoid[Position] {
    lazy val empty = Position(Monoid[Vec2].empty)
    def combine(a: Position, b: Position): Position =
      Position(a.vec |+| b.vec)
  }
}

case class Point(position: Position, velocity: Velocity, mass: Double)

case class Spring(restLenght: Double, stiffness: Double, dampening: Double)

case class Pendulum(anchor: Point, free: Point, spring: Spring)

case class Box(a: Point,
               b: Point,
               c: Point,
               d: Point,
               ab: Spring,
               bc: Spring,
               cd: Spring,
               da: Spring,
               ac: Spring)
