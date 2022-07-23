package example

import example.protocol.{ExampleService, VinSanService}
import zio._
import zio.app.DeriveRoutes
import zio.console._
import zio.magic._

object Backend extends App {

  private val httpApp =
    DeriveRoutes.gen[ExampleService]

  val program = for {
    port <- system.envOrElse("PORT", "8088").map(_.toInt).orElseSucceed(8088)
    _    <- zhttp.service.Server.start(port, httpApp)
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    program
      .injectCustom(ExampleServiceLive.layer)
      .exitCode
  }
}

case class ExampleServiceLive(random: zio.random.Random.Service) extends ExampleService {
  override def magicNumber: UIO[Int] = random.nextInt

  //TODO: Make this a separte service
  //TODO: Allow user to modify these lists
  val pronouns: Seq[String] =
    List("I", "you")
  val verbs: Seq[String] =
    List("eat", "like", "love", "hate")
  val foods: Seq[String] =
    List("pizza", "burger", "chicken", "steak", "salad", "sushi", "ice cream", "cake", "beer", "wine")
  override def genRandomSentence: UIO[String] = for {
    n <- random.nextIntBounded(pronouns.size)
    p = pronouns(n).capitalize
    n <- random.nextIntBounded(verbs.size)
    v = verbs(n)
    n <- random.nextIntBounded(foods.size)
    f = foods(n)
    s = s"$p $v $f."
  } yield s

}

object ExampleServiceLive {
  val layer = (ExampleServiceLive.apply _).toLayer[ExampleService]
}

//case class VinSanServiceLive(random: zio.random.Random.Service) extends VinSanService {
//}
//
//object VinSanServiceLive {
//  val layer = (VinSanServiceLive.apply _).toLayer[VinSanService]
//}
