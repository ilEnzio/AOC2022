package puzzles.Day03

import cats.Functor
import cats.effect.std.Console
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import puzzles.Day03.Day03.charToInt

object IOService extends {

  val fileSource: String = "src/main/scala/inputs/day03"

  /** FS2 version / Finally Tagless version
    */
  def inputStream[F[_]: Files]: Stream[F, String] =
    Files[F]
      .readUtf8Lines(Path(fileSource))

  def stringToRuckSacksPipe[F[_]]: Pipe[F, String, RuckSack] = in =>
    in.map { x =>
      val sack = x.splitAt(x.length / 2)
      RuckSack(sack._1, sack._2)
    }

  // TODO ERG is this going to work?
  def ruckSackToCommonCharsPipe[F[_]]: Pipe[F, RuckSack, String] = in =>
    in.map(_.commonItemDistinct)

  def ruckSackToCommonCharSumPipe[F[_]]: Pipe[F, RuckSack, String] = in =>
    in.map(_.commonItemDistinct)
      .fold(0)((acc, x) => acc + charToInt(x(0)))
      .map(_.toString) // here I have to fold or

  def ruckSacktoStringPipe[F[_]]: Pipe[F, RuckSack, String] = in =>
    in.map(_.toString)

  def ruckSackToConsolePipe[F[_]: Console: Functor]: Pipe[F, RuckSack, Unit] =
    in => in.evalMap(Console[F].println)

  def stringToConsolePipe[F[_]: Console: Functor]: Pipe[F, String, Unit] = in =>
    in.evalMap(Console[F].println)
}
