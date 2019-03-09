package astrac.galileo.demo

import cats.instances.list._
import cats.syntax.compose._
import cats.syntax.foldable._
import cats.syntax.profunctor._
import cats.syntax.semigroup._
import astrac.dimapn.syntax._
import astrac.galileo.dsl._

object Simulations {

  val positionSim: Simulation[Velocity, Position] =
    step(sv => Position(sv.value.vec.scale(sv.delta)))

  val velocitySim: Simulation[Acceleration, Velocity] =
    step(sa => Velocity(sa.value.vec.scale(sa.delta)))

  def uniformAccelerationSim[A](v: Acceleration): Simulation[A, Velocity] =
    const[A](v) >>> velocitySim

  def springFunction(subject: Position,
                     velocity: Velocity,
                     other: Position,
                     spring: Spring): Acceleration = {
    val connection = subject.vec - other.vec
    val pointsDistance = connection.modulo
    val undampened = connection.scale(
      (-spring.stiffness) * (pointsDistance - spring.restLenght))
    val dampenedVelocity = (velocity.vec * connection) / connection.modulo

    Acceleration(undampened - dampenedVelocity.scale(spring.dampening))
  }

  def pointSpringVelocitySim(other: Point,
                             spring: Spring): Simulation[Point, Velocity] =
    focus[Point](p =>
      springFunction(p.position, p.velocity, other.position, spring)) >>> velocitySim

  def simplePointSim(gravity: Acceleration) =
    (
      focus[Point](_.position) |+| positionSim.lmap[Point](_.velocity),
      focus[Point](_.velocity) |+| uniformAccelerationSim[Point](gravity),
      focus[Point](_.mass)
    ).mergeMapN(Point.apply)

  def rigidBodyPointSim(links: List[(Point, Spring)],
                        gravity: Acceleration): Simulation[Point, Point] =
    describe[Point] { p =>
      (
        focus[Point](_.position) |+| positionSim.lmap[Point](_.velocity),
        focus[Point](_.velocity) |+| (const[Point](
          gravity |+| links.foldMap(l =>
            springFunction(p.position, p.velocity, l._1.position, l._2))) >>> velocitySim),
        focus[Point](_.mass)
      ).dimapN((p: Point) => (p, p, p))(Point.apply)
    }

  def pendulumSim(gravity: Acceleration) = describe[Pendulum] { p =>
    (
      pass[Point],
      rigidBodyPointSim(List((p.anchor, p.spring)), gravity),
      pass[Spring]
    ).dimapN((p: Pendulum) => (p.anchor, p.free, p.spring))(Pendulum.apply)
  }

  val gravity = Acceleration(Vec2(0, -10))

  val boxSim = describe[Box] { b =>
    (
      rigidBodyPointSim(List((b.b, b.ab), (b.c, b.ac), (b.d, b.da)), gravity),
      rigidBodyPointSim(List((b.a, b.ab), (b.c, b.bc)), gravity),
      rigidBodyPointSim(List((b.a, b.ac), (b.b, b.bc), (b.d, b.cd)), gravity),
      rigidBodyPointSim(List((b.a, b.da), (b.c, b.cd)), gravity),
      pass[Spring],
      pass[Spring],
      pass[Spring],
      pass[Spring],
      pass[Spring]
    ).dimapN[Box, Box](b => (b.a, b.b, b.c, b.d, b.ab, b.bc, b.cd, b.da, b.ac))(
      Box.apply)
  }
}
