package example

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveSvgElement
//import example.Boxes._
import animus._
import example.Utils._
import org.scalajs.dom.svg.RectElement

import scala.math._

object Frontend {

  /** Used to store mouse position at all times */
  private var mousePosition = (0.0, 0.0)

  /** Used to store mouse position; None when onClick event happened odd number of times */
  private var maybeMousePosition: Option[(Double, Double)] = None

  /** Create a reactive Svg element that moves in the pattern specified by the x and y iterators */
  def movingBox(
      xIterator: Iterator[Double],
      yIterator: Iterator[Double],
      interval: Int,
      delay: Int,
      size: Int,
      shift: (Int, Int)
  ): ReactiveSvgElement[RectElement] = {
    def $cornersX(delay: Int): Signal[Double] = makeSignal(
      maybeMousePosition match {
        case None      => xIterator.next() * size + shift._1
        case Some(pos) => pos._1 - 25
      },
      interval,
      delay
    )
    def $cornersY(delay: Int): Signal[Double] =
      makeSignal(
        maybeMousePosition match {
          case None      => yIterator.next() * size + shift._2
          case Some(pos) => pos._2 - 25
        },
        interval,
        delay
      )
    def $colors(delay: Int): Signal[(Double, Double, Double)] =
      makeSignal(randomRGB match { case (r, g, b) => (r.toDouble, g.toDouble, b.toDouble) }, 1000, delay)
    svg.rect(
      svg.width("50px"),
      svg.height("50px"),
      svg.fill <-- $colors(delay).spring.map { case (r, g, b) => s"rgb($r, $g, $b)" },
      svg.x <-- $cornersX(delay).spring.px,
      svg.y <-- $cornersY(delay).spring.px
    )
  }

  /** A single box moving in a square like pattern */
  def boxMovingInSquare(
      interval: Int,
      delay: Int,
      size: Int,
      shift: (Int, Int)
  ): ReactiveSvgElement[RectElement] =
    movingBox(
      iterateContinously(List(0, 1, 1, 0)),
      iterateContinously(List(0, 0, 1, 1)),
      interval,
      delay,
      size,
      shift
    )

  /** A single box moving in a circle pattern */
  def boxMovingInCircle(
      interval: Int,
      delay: Int,
      size: Int,
      shift: (Int, Int),
      steps: Int = 100
  ): ReactiveSvgElement[RectElement] =
    movingBox(
      iterateContinously((0 to steps).map(i => -cos(i * 2 * Pi / steps))),
      iterateContinously((0 to steps).map(i => sin(i * 2 * Pi / steps))),
      interval,
      delay,
      size,
      shift
    )

  def view: Div =
    div(
      windowEvents.onMouseMove --> { e =>
        {
          val bounds = e.target.getBoundingClientRect()
          mousePosition = (e.pageX - bounds.left) -> (e.pageY - bounds.top)
        }
      },
      windowEvents.onClick --> { e =>
        maybeMousePosition match {
          case None => maybeMousePosition = Some(mousePosition)
          case _    => maybeMousePosition = None
        }
      },
//      debugView("Generate a random sentence", client.genRandomSentence),
      svg.svg(
        svg.width("1000px"),
        svg.height("1000px"),
        (0 to 100).map(x => boxMovingInSquare(1000, x * 100, 500, (0, 0))) ++
          (0 to 30).map(x => boxMovingInCircle(50, x * 200, 200, (250, 250))) ++
          (0 to 25).map(x => boxMovingInSquare(1000, x * 200, 100, (200, 200)))
      )
    )
}
