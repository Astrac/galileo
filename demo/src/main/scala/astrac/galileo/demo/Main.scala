package astrac.galileo.demo

import cats.effect.IO
import cats.implicits._
import fs2.Stream
import scalajs.js
import js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import Scenes._

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
      ).render
    )

    run()
  }

  val graphics = simCanvas
    .getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  val pendulum = Pendulum(
    Point(Position(350, 10), Velocity(0, 0), Acceleration(0, 0), Mass(1)),
    Point(Position(550, 10), Velocity(0, 0), Acceleration(0, 0), Mass(1)),
    Spring(300, 50, 5)
  )

  val gravity = Force(0, 150)

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
    graphics.arc(p.position.vector.x, p.position.vector.y, 5, 0, 2 * math.Pi)
    graphics.fillStyle = "green"
    graphics.fill()
    graphics.strokeStyle = "#666666"
    graphics.stroke()
    graphics.closePath()
  }

  def drawLine(p1: Position, p2: Position): IO[Unit] = IO {
    graphics.beginPath()
    graphics.lineWidth = 1.0
    graphics.moveTo(p1.vector.x, p1.vector.y)
    graphics.lineTo(p2.vector.x, p2.vector.y)
    graphics.strokeStyle = "#666666"
    graphics.stroke()
    graphics.closePath()
  }

  def drawPendulum(p: Pendulum): IO[Unit] =
    drawPoint(p.anchor) >> drawPoint(p.free) >> drawLine(
      p.anchor.position,
      p.free.position
    )

  def run(): Unit = {
    val tick = Stream.fixedRate(40.millis)

    val sim = Scenes.pendulum(gravity)

    val pendulumRedraws = Stream
      .iterate[IO, Pendulum](pendulum)(p => sim(Step(p, Time(deltaT))))
      .evalMap(p => clear >> drawPendulum(p))

    (tick.zipRight(pendulumRedraws)).compile.drain.unsafeRunAsyncAndForget
  }
}
