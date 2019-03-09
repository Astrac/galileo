package astrac.galileo.physics

import cats.Comonad
import cats.syntax.profunctor._
import astrac.galileo.Simulation
import quantities._
import spire.algebra.{Field, NRoot}
import spire.syntax.vectorSpace._

object Newtonian {
  case class Parameters[A](
      kinematics: Kinematics.Parameters[A],
      mass: Mass[A]
  )

  def nonConservative[F[_]: Comonad, A: Field: NRoot](
      interactionForces: (Position[A], Velocity[A]) => Force[A],
      time: F[_] => F[Time[A]]
  ): Simulation[F, Parameters[A], Parameters[A]] =
    Simulation.describe[F, Parameters[A]] { current =>
      Kinematics
        .nonConservative[F, A]({ (p, v) =>
          Acceleration(interactionForces(p, v).vector :/ current.mass.value)
        }, time)
        .dimap((ps: Parameters[A]) => ps.kinematics)(
          Parameters[A](_, current.mass)
        )
    }

  def conservative[F[_]: Comonad, A: Field: NRoot](
      interactionForces: (Position[A]) => Force[A],
      time: F[_] => F[Time[A]]
  ): Simulation[F, Parameters[A], Parameters[A]] =
    nonConservative[F, A]((p, _) => interactionForces(p), time)
}
