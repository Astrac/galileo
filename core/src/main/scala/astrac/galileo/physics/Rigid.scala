package astrac.galileo.physics

import astrac.galileo.Simulation
import cats.Comonad
import cats.instances.list._
import cats.syntax.foldable._
import Quantities._
import spire.algebra.{Field, NRoot}
import spire.syntax.all._

object Rigid {
  case class Spring[A](
      restLenght: Length[A],
      stiffness: Stiffness[A],
      damping: Damping[A]
  )

  def springFunction[A: Field: NRoot](
      subject: Position[A],
      velocity: Velocity[A],
      other: Position[A],
      spring: Spring[A]
  ): Force[A] = {
    val field = Field[A]
    val connection = subject - other
    val pointsDistance = connection.norm
    val connectionVersor = connection.normalize
    val undampened = Force(
      (connectionVersor :* ((-spring.stiffness.value) * (pointsDistance - spring.restLenght.value))).vector
    )

    val dampening = Force(
      connectionVersor.vector :* (velocity.vector
        .dot(connectionVersor.vector) * spring.damping.value)
    )

    undampened - dampening
  }

  def springs[F[_]: Comonad, A: Field: NRoot](
      links: List[(Position[A], Spring[A])],
      time: F[_] => F[Time[A]]
  ): Simulation[F, Newtonian.Parameters[A], Newtonian.Parameters[A]] =
    Newtonian.nonConservative[F, A]({ (p, v) =>
      links.foldMap(l => springFunction(p, v, l._1, l._2))
    }, time)

  def spring[F[_]: Comonad, A: Field: NRoot](
      otherPosition: Position[A],
      spring: Spring[A],
      time: F[_] => F[Time[A]]
  ) =
    springs[F, A](List((otherPosition, spring)), time)
}
