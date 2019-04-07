package astrac.galileo.physics

import cats.Comonad
import cats.syntax.comonad._
import astrac.galileo.Simulation
import Quantities._
import spire.algebra.{Field, NRoot}
import spire.syntax.vectorSpace._
import Predef.{any2stringadd => _}

object Kinematics {

  case class Parameters[A](
      position: Position[A],
      velocity: Velocity[A],
      acceleration: Acceleration[A]
  )

  def nonConservative[F[_]: Comonad, A: Field: NRoot](
      interactionPotential: (Position[A], Velocity[A]) => Acceleration[A],
      time: F[_] => F[Time[A]]
  ): Simulation[F, Parameters[A], Parameters[A]] =
    Simulation { (fState: F[Parameters[A]]) =>
      val current = fState.extract
      val dt = time(fState).extract
      val half = Field[A].fromDouble(0.5)

      coercibleVectorSpace2[Mass[A], Velocity[A], A]

      val velocityHalfstep = current.velocity +
        Velocity((current.acceleration :* dt :* half).vector)

      val newPosition =
        current.position + Position((velocityHalfstep :* dt).vector)

      val newAcceleration =
        interactionPotential(newPosition, velocityHalfstep)

      val newVelocity =
        current.velocity + Velocity(
          ((current.acceleration + newAcceleration) :* half :* dt).vector
        )

      Parameters(newPosition, newVelocity, newAcceleration)
    }

  def conservative[F[_]: Comonad, A: Field: NRoot](
      interactionPotential: Position[A] => Acceleration[A],
      time: F[_] => F[Time[A]]
  ): Simulation[F, Parameters[A], Parameters[A]] =
    nonConservative[F, A]((p, _) => interactionPotential(p), time)
}
