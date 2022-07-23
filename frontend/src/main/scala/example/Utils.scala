package example

import com.raquo.airstream.timing.PeriodicEventStream
import com.raquo.laminar.api.L._
import example.protocol.ExampleService
import zio.app.DeriveClient
import zio.{UIO, ZIO, _}

import scala.util.Random

object Utils {

  val runtime: Runtime[zio.ZEnv] = Runtime.default
  val client: ExampleService     = DeriveClient.gen[ExampleService]

  def randomRGB: (Int, Int, Int) = (Random.nextInt(255), Random.nextInt(255), Random.nextInt(255))
  def randomColor: String        = s"rgb$randomRGB"
  def randomHexColor: String     = "#" + Random.nextInt(0xffffff).toHexString

  def iterateContinously[A](iterable: Iterable[A]): Iterator[A] = LazyList
    .continually(iterable)
    .flatten
    .iterator

  def defaultSignal(interval: Int, delay: Int): Signal[Int]                   = EventStream.periodic(interval).delay(delay).toSignal(0)
  def makeSignal[T](f: => T, interval: Int = 1000, delay: Int = 0): Signal[T] = defaultSignal(interval, delay).mapTo(f)

  def every[A](a: => A, ms: => Int = 300): Signal[A] =
    new PeriodicEventStream[A](
      initial = a,
      next = _ => Some((a, ms)),
      true,
      true
    ).toSignal(a)

  def debugView[A](name: String, effect: => UIO[A]): Div = {
    val output = Var(List.empty[String])
    div(
      h3(name),
      children <-- output.signal.map { strings =>
        strings.map(x => div(x, backgroundColor(randomColor)))
      },
      onClick --> { _ =>
        runtime.unsafeRunAsync_ {
          effect.tap { a => ZIO.succeed(output.update(_.prepended(a.toString))) }
        }
      }
    )
  }
}
