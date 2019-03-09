package astrac.galileo.demo

import astrac.galileo.dsl.Step
import cats.effect.IO
import cats.implicits._
import fs2.Stream
import scalajs.js
import js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@JSExportTopLevel("astrac.galileo.Demo")
object Demo {
  import scalatags.JsDom.all._

  implicit val timer = IO.timer(global)

  val canvasWidth = 700
  val canvasHeight = 500

  val simCanvas = canvas(
    margin := "10 0",
    attr("width") := canvasWidth,
    attr("height") := canvasHeight
  ).render

  def main(args: Array[String]): Unit = {}

  @JSExport
  def ui(): Unit = {
    dom.document.body.appendChild(
      div(
        margin := 10,
        h1("Galileo demo"),
        simCanvas
      ).render)

    run()
  }

  val graphics = simCanvas
    .getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  val point = Point(Position(Vec2(350, 50)), Velocity(Vec2(0, 0)), 123)

  val pendulum = Pendulum(
    Point(Position(Vec2(300, 50)), Velocity(Vec2(0, 0)), 123),
    Point(Position(Vec2(350, 300)), Velocity(Vec2(0, 0)), 123),
    Spring(300, 0.1, 3))

  val gravity = Acceleration(Vec2(0, 500))

  val deltaT = 0.025

  val clear: IO[Unit] = IO {
    graphics.beginPath()
    graphics.lineWidth = 1.0
    graphics.fillStyle = "#222222"
    graphics.fillRect(0, 0, simCanvas.width.toDouble, simCanvas.height.toDouble)
    graphics.closePath()
  }

  def drawPoint(p: Point): IO[Unit] = IO {
    graphics.beginPath()
    graphics.lineWidth = 1.0
    graphics.arc(p.position.vec.x, p.position.vec.y, 5, 0, 2 * math.Pi)
    graphics.fillStyle = "green"
    graphics.fill()
    graphics.strokeStyle = "#666666"
    graphics.stroke()
    graphics.closePath()
  }

  def drawLine(p1: Position, p2: Position): IO[Unit] = IO {
    graphics.beginPath()
    graphics.lineWidth = 1.0
    graphics.moveTo(p1.vec.x, p1.vec.y)
    graphics.lineTo(p2.vec.x, p2.vec.y)
    graphics.strokeStyle = "#666666"
    graphics.stroke()
    graphics.closePath()
  }

  def drawPendulum(p: Pendulum): IO[Unit] =
    drawPoint(p.anchor) >> drawPoint(p.free) >> drawLine(p.anchor.position,
                                                         p.free.position)

  def run(): Unit = {
    val tick = Stream.fixedRate(40.millis)

    val pendulumRedraws = Stream
      .iterate[IO, Pendulum](pendulum)(p =>
        Simulations.pendulumSim(gravity)(Step(p, deltaT)))
      .evalMap(p => clear >> drawPendulum(p))

    (tick.zipRight(pendulumRedraws)).compile.drain.unsafeRunAsyncAndForget
  }
}
