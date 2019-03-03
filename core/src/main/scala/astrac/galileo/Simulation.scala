package astrac.galileo

import cats.{Comonad, Monad, Monoid}
import cats.arrow.Arrow
import cats.data.Cokleisli
import cats.syntax.arrow._
import cats.syntax.flatMap._
import cats.syntax.profunctor._
import cats.syntax.semigroup._

class SimulationModule[Input] {

  case class Step[A](value: A, delta: Input)

  object Step {
    implicit val comonadInstance = new Comonad[Step] {
      def extract[A](s: Step[A]): A = s.value
      def map[A, B](s: Step[A])(f: A => B): Step[B] = s.copy(value = f(s.value))
      def coflatMap[A, B](s: Step[A])(f: Step[A] => B): Step[B] =
        s.copy(value = f(s))
    }
  }

  case class Simulation[A, B](stepCokleisli: Cokleisli[Step, A, B])

  object Simulation {
    implicit val arrowInstance = new Arrow[Simulation] {
      def lift[A, B](f: A => B): Simulation[A, B] =
        Simulation(s => f(s.value))

      def compose[A, B, C](f: Simulation[B, C],
                           g: Simulation[A, B]): Simulation[A, C] =
        Simulation(f.stepCokleisli.compose(g.stepCokleisli))

      def first[A, B, C](f: Simulation[A, B]): Simulation[(A, C), (B, C)] =
        Simulation(f.stepCokleisli.first[C])
    }

    implicit def monoidInstance[A, B: Monoid]: Monoid[Simulation[A, B]] =
      new Monoid[Simulation[A, B]] {
        def empty: Simulation[A, B] = Simulation(_ => Monoid[B].empty)
        def combine(a: Simulation[A, B],
                    b: Simulation[A, B]): Simulation[A, B] =
          (a &&& b).rmap(bb => bb._1 |+| bb._2)
      }

    implicit def monadInstance[A]: Monad[Simulation[A, ?]] =
      new Monad[Simulation[A, ?]] {
        def pure[B](b: B): Simulation[A, B] = Simulation.focus[A](_ => b)
        def flatMap[B, C](fb: Simulation[A, B])(f: B => Simulation[A, C]) =
          Simulation(fb.stepCokleisli.flatMap(b => f(b).stepCokleisli))
        def tailRecM[B, C](b: B)(
            f: B => Simulation[A, Either[B, C]]): Simulation[A, C] =
          Simulation(
            Monad[Cokleisli[Step, A, ?]].tailRecM(b)(b => f(b).stepCokleisli))
      }

    def apply[A, B](f: Step[A] => B): Simulation[A, B] =
      Simulation(Cokleisli(f))

    class PartiallyAppliedFocus[A] {
      def apply[B](f: A => B): Simulation[A, B] = Arrow[Simulation].lift(f)
    }

    def focus[A] = new PartiallyAppliedFocus[A]

    def id[A]: Simulation[A, A] = focus(identity[A])

    def describe[A](f: A => Simulation[A, A]): Simulation[A, A] =
      Simulation.id[A].flatMap(f)

    def iterateFixedInput[A](input: Input, from: A, sim: Simulation[A, A]) =
      Iterator.iterate(from)(a => sim.stepCokleisli.run(Step(a, input)))

  }

  object dsl {
    type Simulation[A, B] = SimulationModule.this.Simulation[A, B]
    type Step[A] = SimulationModule.this.Step[A]

    def pass[A]: Simulation[A, A] = Simulation.id[A]

    def focus[A] = Simulation.focus[A]

    def describe[A](f: A => Simulation[A, A]): Simulation[A, A] =
      Simulation.describe(f)

    def step[A, B](f: Step[A] => B): Simulation[A, B] =
      Simulation(f)

    class PartiallyAppliedConst[A] {
      def apply[B](b: B): Simulation[A, B] = step(_ => b)
    }

    def const[A]: PartiallyAppliedConst[A] =
      new PartiallyAppliedConst[A]
  }
}
