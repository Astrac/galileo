package astrac.galileo.demo

import astrac.dimapn.syntax._
import astrac.galileo.physics._
import astrac.galileo.physics.Quantities
import astrac.galileo.Simulation
import cats.Comonad
import cats.implicits._
import spire.std.double._

object Scenes extends Quantities[Double] {
  case class Step[A](value: A, time: Time)

  object Step {
    implicit val comonadInstance: Comonad[Step] =
      new Comonad[Step] {
        override def extract[A](s: Step[A]): A = s.value
        override def map[A, B](s: Step[A])(f: A => B): Step[B] =
          s.copy(value = f(s.value))
        override def coflatMap[A, B](s: Step[A])(f: Step[A] => B): Step[B] =
          s.copy(value = f(s))

      }
  }

  case class Point(
      position: Position,
      velocity: Velocity,
      acceleration: Acceleration,
      mass: Mass
  )

  case class Pendulum(anchor: Point, free: Point, spring: Spring)

  val stepTime: Step[_] => Step[Time] = _.coflatMap(_.time)

  val pointToNewtonian: Point => Newtonian.Parameters[Double] = p =>
    Newtonian.Parameters(
      Kinematics.Parameters(p.position, p.velocity, p.acceleration),
      p.mass
    )

  val pointFromNewtonian: Newtonian.Parameters[Double] => Point = np =>
    Point(
      np.kinematics.position,
      np.kinematics.velocity,
      np.kinematics.acceleration,
      np.mass
    )

  val pendulumSpring: Simulation[Step, Pendulum, Point] =
    Simulation.id[Step, Pendulum].flatMap { pendulum =>
      Rigid
        .spring[Step, Double](
          pendulum.anchor.position,
          pendulum.spring,
          stepTime
        )
        .dimap((p: Pendulum) => pointToNewtonian(p.free))(pointFromNewtonian)
    }

  def pointConstantForce(force: Force): Simulation[Step, Point, Point] =
    Newtonian
      .conservative[Step, Double](_ => force, stepTime)
      .dimap(pointToNewtonian)(pointFromNewtonian)

  def pendulum(force: Force) =
    (
      Simulation.focus[Step, Pendulum](_.anchor),
      pendulumSpring >>> pointConstantForce(force),
      Simulation.focus[Step, Pendulum](_.spring)
    ).mergeN.map(Pendulum.tupled)
}
