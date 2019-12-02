import java.io.{IOException, InputStream}

import zio.{Managed, UIO, ZIO}
import zio.stream.{ZSink, ZStream}

package object freskog {

  def openStream(path: String): UIO[InputStream] =
    ZIO.effect(this.getClass.getClassLoader.getResourceAsStream(path)).orDie

  def closeStream(is: InputStream): UIO[Unit] =
    ZIO.effectTotal(is.close())

  def inputStreamFromFile(path: String): Managed[Nothing, InputStream] =
    Managed.make(openStream(path))(closeStream)

  def decodeAsString(inputStream: InputStream): ZStream[Any, IOException, String] =
    ZStream.fromInputStream(inputStream).chunks.aggregate(ZSink.utf8DecodeChunk)


  def linesFrom(inputStream: InputStream): ZStream[Any, IOException, Long] =
    decodeAsString(inputStream)
      .aggregate(ZSink.splitLines)
      .mapConcatChunk(identity)
      .takeUntil(_.isEmpty)
      .map(_.toLong)

  def splitByCommas(inputStream: InputStream): ZStream[Any, IOException, Int] =
    decodeAsString(inputStream)
    .aggregate(ZSink.splitOn(","))
    .mapConcatChunk(identity)
    .takeUntil(_.isEmpty)
    .map(_.toInt)

  def decodeLines(path:String): ZStream[Any, IOException, Long] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(linesFrom)

  def decodeCommaSeparated(path:String): ZStream[Any, IOException, Int] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(splitByCommas)

  def decodeCommaSeparatedAsArray(path:String): ZIO[Any, IOException, Array[Int]] =
    decodeCommaSeparated(path).runCollect.map(_.toArray)

}
