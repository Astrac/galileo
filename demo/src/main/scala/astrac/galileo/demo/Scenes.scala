package astrac.galileo.demo

import astrac.dimapn.syntax._
import astrac.galileo.physics._
import astrac.galileo.Simulation
import cats.Comonad
import cats.implicits._
import spire.std.double._

object Scenes {
  type Position = quantities.Position[Double]
  type Velocity = quantities.Velocity[Double]
  type Acceleration = quantities.Acceleration[Double]
  type Mass = quantities.Mass[Double]
  type Spring = Rigid.Spring[Double]
  type Time = quantities.Time[Double]

  def Position(a: Double, b: Double) =
    quantities.Position(Vector(a, b))

  def Velocity(a: Double, b: Double) =
    quantities.Velocity(Vector(a, b))

  def Acceleration(a: Double, b: Double) =
    quantities.Acceleration(Vector(a, b))

  def Force(a: Double, b: Double) =
    quantities.Force(Vector(a, b))

  def Mass(a: Double) = quantities.Mass(a)
  def Time(a: Double) = quantities.Time(a)

  def Spring(a: Double, b: Double, c: Double) =
    Rigid.Spring(a, b, c)

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

  def pointConstantForce(
      force: quantities.Force[Double]
  ): Simulation[Step, Point, Point] =
    Newtonian
      .conservative[Step, Double](_ => force, stepTime)
      .dimap(pointToNewtonian)(pointFromNewtonian)

  def pendulum(force: quantities.Force[Double]) =
    (
      Simulation.focus[Step, Pendulum](_.anchor),
      pendulumSpring >>> pointConstantForce(force),
      Simulation.focus[Step, Pendulum](_.spring)
    ).mergeN.map(Pendulum.tupled)
}
