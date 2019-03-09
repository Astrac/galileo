package astrac.galileo

import cats.Comonad

object dsl {

  case class Step[A](value: A, delta: Double)

  object Step {
    implicit val comonadInstance = new Comonad[Step] {
      def extract[A](s: Step[A]): A = s.value
      def map[A, B](s: Step[A])(f: A => B): Step[B] = s.copy(value = f(s.value))
      def coflatMap[A, B](s: Step[A])(f: Step[A] => B): Step[B] =
        s.copy(value = f(s))
    }
  }

  type Simulation[A, B] = astrac.galileo.Simulation[Step, A, B]

  def pass[A]: Simulation[A, A] = Simulation.id[Step, A]

  def focus[A] = Simulation.focus[Step, A]

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
