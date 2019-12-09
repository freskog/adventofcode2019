import java.io.{IOException, InputStream}

import zio.{Managed, UIO, ZIO, stream}
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

  def linesFrom(inputStream: InputStream): ZStream[Any, IOException, String] =
    decodeAsString(inputStream)
      .aggregate(ZSink.splitLines)
      .mapConcatChunk(identity)
      .takeUntil(_.isEmpty)


  def splitByCommas(inputStream: InputStream): ZStream[Any, IOException, String] =
    decodeAsString(inputStream)
      .aggregate(ZSink.splitOn(","))
      .mapConcatChunk(identity)
      .takeUntil(_.isEmpty)


  def splitCommasWithMultipleLines(inputStream: InputStream): ZStream[Any, IOException, List[String]] =
    decodeAsString(inputStream).aggregate(ZSink.splitLines).flatMap(ZStream.fromChunk).map(_.split(",").toList)

  def decodeLines(path: String): ZStream[Any, IOException, String] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(linesFrom)

  def decodeCommaSeparated(path: String): ZStream[Any, IOException, String] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(splitByCommas)

  def decodeCommaSeparatedAsArray(path: String): ZIO[Any, IOException, Array[Int]] =
    decodeCommaSeparated(path).map(_.toInt).runCollect.map(_.toArray)

  def decodeCommaSeparatedAsMap(path:String): ZIO[Any, IOException, Map[BigInt, BigInt]] =
    decodeCommaSeparated(path).map(BigInt(_)).runCollect.map(
      _.zipWithIndex.foldLeft(Map.empty[BigInt,BigInt]) {
        case (acc, (v,idx)) => acc.updated(BigInt(idx), v)
      }
    )

  def decodeCommaSeparatedOnMultipleLines(path:String): ZIO[Any, IOException, List[List[String]]] =
    ZStream.managed(inputStreamFromFile(path)).flatMap(splitCommasWithMultipleLines).runCollect

}
