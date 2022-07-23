package example.protocol

import zio.UIO

trait VinSanService {
  def genRandomSentence: UIO[String]
}
