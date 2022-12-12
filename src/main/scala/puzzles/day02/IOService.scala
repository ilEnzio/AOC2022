package puzzles.day02

import fs2.{Pipe, Stream}
import fs2.io.file.{Files, Path}
import puzzles.day02.Day02.{Move, MyGameResult, RockPaperScissors}

// TODO Change this trait
object IOService extends RockPaperScissors {

  override val fileSource: String = "src/main/scala/inputs/day02"

  /** FS2 version / Finally Tagless version
    */
  def inputStream[F[_]: Files]: Stream[F, String] =
    Files[F]
      .readUtf8Lines(Path(fileSource))

  def stringToGameMovesPipe[F[_]]: Pipe[F, String, (Move, Move, MyGameResult)] =
    inStream => {
      for {
        gameMoves <- inStream.fold(List.empty[(Move, Move, MyGameResult)]) {
          (s, v) =>
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
  def movesToScorePipe[F[_]]: Pipe[F, (Move, Move, MyGameResult), Int] =
    inStream =>
      for {
        (oppMove, myMove) <- inStream.map { case (x, y, _) => (x, y) }
        result = MyGameResult.playGame(oppMove, myMove)
      } yield MyGameResult.scoreGame(myMove, result)

  def movesToRiggedScoresPipe[F[_]]: Pipe[F, (Move, Move, MyGameResult), Int] =
    inStream =>
      for {
        (oppMove, result) <- inStream.map { case (x, _, y) => (x, y) }
      } yield MyGameResult.scoreFixedGame(oppMove, result)

}
