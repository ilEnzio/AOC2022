package puzzles.day02

import cats.Functor
import cats.effect.std.Console
import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import puzzles.day02.Day02.{Move, MyGameResult, MyMove, OpponentMove}

import scala.io.Source

// TODO Change this trait
object IOService {

  val fileSource: String = "src/main/scala/inputs/day02"

  def allGameMoves: List[(Move[OpponentMove], Move[MyMove], MyGameResult)] =
    Source
      .fromFile(fileSource)
      .getLines()
      .mkString("")
      .grouped(3)
      .toList
      .map {
        _.toCharArray match {
          case Array(h, _, t) =>
            (
              MyGameResult.opponentMoveMap(h.toString),
              MyGameResult.myMoveMap(t.toString),
              MyGameResult.resultMap(t.toString)
            )
        }
      }

  /** FS2 version / Finally Tagless version
    */
  def inputStream[F[_]: Files]: Stream[F, String] =
    Files[F]
      .readUtf8Lines(Path(fileSource))

  def stringToGameMovesPipe[F[_]]
    : Pipe[F, String, (Move[OpponentMove], Move[MyMove], MyGameResult)] =
    inStream => {
      for {
        gameMoves <- inStream.fold(
          List.empty[(Move[OpponentMove], Move[MyMove], MyGameResult)]
        ) { (s, v) =>
          v.toCharArray match {
            case Array(h, _, t) =>
              (
                MyGameResult.opponentMoveMap(h.toString),
                MyGameResult.myMoveMap(t.toString),
                MyGameResult.resultMap(t.toString)
              ) :: s
          }
        }
        stream <- Stream.emits(gameMoves)
      } yield stream
    }

  // TODO these seem wrong; they should be accumulating the total??
//  def movesToScorePipe[F[_]]: Pipe[F, (Move, Move, MyGameResult), Int] =
//    inStream =>
//      for {
//        (oppMove, myMove) <- inStream.map { case (x, y, _) => (x, y) }
//        result = MyGameResult.playGame(oppMove, myMove)
//      } yield MyGameResult.scoreGame(myMove, result)

  def movesToScoreTotalPipe[F[_]]
    : Pipe[F, (Move[OpponentMove], Move[MyMove], MyGameResult), Int] =
    inStream =>
      inStream.fold(0) { case (s, v) =>
        val (oppMove, myMove, _) = v
        val result               = MyGameResult.playGame(oppMove, myMove)
        MyGameResult.scoreGame(myMove, result) + s
      }

//  def movesToRiggedScorePipe[F[_]]: Pipe[F, (Move, Move, MyGameResult), Int] =
//    inStream =>
//      for {
//        (oppMove, result) <- inStream.map { case (x, _, y) => (x, y) }
//      } yield MyGameResult.scoreFixedGame(oppMove, result)

  def movesToRiggedScoreTotalPipe[F[_]]
    : Pipe[F, (Move[OpponentMove], Move[MyMove], MyGameResult), Int] =
    inStream =>
      inStream.fold(0) { case (s, v) =>
        val (oppMove, _, result) = v
        MyGameResult.scoreFixedGame(oppMove, result) + s
      }

  def toConsolePipe[F[_]: Console: Functor]: Pipe[F, Int, Unit] = inStream =>
    inStream.evalMap(Console[F].println(_))

}
