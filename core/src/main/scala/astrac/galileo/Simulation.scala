package astrac.galileo

import cats.{Comonad, Monad, Monoid}
import cats.arrow.Arrow
import cats.data.Cokleisli
import cats.syntax.arrow._
import cats.syntax.comonad._
import cats.syntax.flatMap._
import cats.syntax.profunctor._
import cats.syntax.semigroup._

case class Simulation[F[_], A, B](stepCokleisli: Cokleisli[F, A, B]) {
  def apply(s: F[A]): B = stepCokleisli.run(s)
}

object Simulation {
  implicit def arrowInstance[F[_]: Comonad] = new Arrow[Simulation[F, ?, ?]] {
    def lift[A, B](f: A => B): Simulation[F, A, B] =
      Simulation(s => f(s.extract))

    def compose[A, B, C](
        f: Simulation[F, B, C],
        g: Simulation[F, A, B]
    ): Simulation[F, A, C] =
      Simulation(f.stepCokleisli.compose(g.stepCokleisli))

    def first[A, B, C](f: Simulation[F, A, B]): Simulation[F, (A, C), (B, C)] =
      Simulation(f.stepCokleisli.first[C])
  }

  implicit def monoidInstance[F[_]: Comonad, A, B: Monoid]
      : Monoid[Simulation[F, A, B]] =
    new Monoid[Simulation[F, A, B]] {
      def empty: Simulation[F, A, B] = Simulation(_ => Monoid[B].empty)
      def combine(
          a: Simulation[F, A, B],
          b: Simulation[F, A, B]
      ): Simulation[F, A, B] =
        (a &&& b).rmap(bb => bb._1 |+| bb._2)
    }

  implicit def monadInstance[F[_]: Comonad, A]: Monad[Simulation[F, A, ?]] =
    new Monad[Simulation[F, A, ?]] {
      def pure[B](b: B): Simulation[F, A, B] =
        Simulation.focus[F, A]((_: A) => b)
      def flatMap[B, C](fb: Simulation[F, A, B])(f: B => Simulation[F, A, C]) =
        Simulation(fb.stepCokleisli.flatMap(b => f(b).stepCokleisli))
      def tailRecM[B, C](
          b: B
      )(f: B => Simulation[F, A, Either[B, C]]): Simulation[F, A, C] =
        Simulation(
          Monad[Cokleisli[F, A, ?]].tailRecM(b)(b => f(b).stepCokleisli)
        )
    }

  def apply[F[_], A, B](f: F[A] => B): Simulation[F, A, B] =
    Simulation(Cokleisli(f))

  class PartiallyAppliedFocus[F[_], A] {
    def apply[B](f: A => B)(implicit F: Comonad[F]): Simulation[F, A, B] =
      Arrow[Simulation[F, ?, ?]].lift(f)
  }

  def focus[F[_], A] = new PartiallyAppliedFocus[F, A]

  def id[F[_]: Comonad, A]: Simulation[F, A, A] = focus(identity[A])

  def describe[F[_]: Comonad, A](
      f: A => Simulation[F, A, A]
  ): Simulation[F, A, A] =
    Simulation.id[F, A].flatMap(f)
}
