package puzzles.day02

import cats.Monad
import cats.data.Reader
import cats.effect.IO
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import fs2.{Pipe, Stream}
import fs2.io.file.Files
import puzzles.day02.Day02.RockPaperScissors._
import puzzles.day02.IOService._
import puzzles.day02.IOService.toConsolePipe

/** --- Day 2: Rock Paper Scissors ---
  * The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage, a giant Rock Paper Scissors tournament is already in progress.
  *
  * Rock Paper Scissors is a game between two players. Each game contains many rounds; in each round, the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. Then, a winner for that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw.
  *
  * Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win. "The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.
  *
  * The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must have been carefully chosen.
  *
  * The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
  *
  * Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you would get if you were to follow the strategy guide.
  *
  * For example, suppose you were given the following strategy guide:
  *
  * A Y
  * B X
  * C Z
  * This strategy guide predicts and recommends the following:
  *
  * In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
  * In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
  * The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.
  * In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6).
  *
  * What would your total score be if everything goes exactly according to your strategy guide?
  *
  * Your puzzle answer was 13526.
  *
  * --- Part Two ---
  * The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"
  *
  * The total score is still calculated in the same way, but now you need to figure out what shape to choose so the round ends as indicated. The example above now goes like this:
  *
  * In the first round, your opponent will choose Rock (A), and you need the round to end in a draw (Y), so you also choose Rock. This gives you a score of 1 + 3 = 4.
  * In the second round, your opponent will choose Paper (B), and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
  * In the third round, you will defeat your opponent's Scissors with Rock for a score of 1 + 6 = 7.
  * Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12.
  *
  * Following the Elf's instructions for the second column, what would your total score be if everything goes exactly according to your strategy guide?
  */

object Day02 {

  sealed trait Move[A] {
    val value: Int
  }
  sealed trait Player
  trait Villain extends Player
  trait Hero    extends Player

  case class Rock[A <: Player]() extends Move[A] {
    override val value: Int = 1
  }
  case class Paper[A <: Player]() extends Move[A] {
    override val value: Int = 2
  }
  case class Scissors[A <: Player]() extends Move[A] {
    override val value: Int = 3
  }

  sealed trait MyGameResult {
    val points: Int
  }

  case object Win extends MyGameResult {
    override val points: Int = 6
  }

  case object Lose extends MyGameResult {
    override val points: Int = 0
  }

  case object Draw extends MyGameResult {
    override val points: Int = 3
  }

  object MyGameResult {

    val all = List(Win, Lose, Draw)

    val opponentMoveMap: Map[String, Move[Villain]] =
      Map(
        "A" -> Rock[Villain],
        "B" -> Paper[Villain],
        "C" -> Scissors[Villain]
      )

    val myMoveMap: Map[String, Move[Hero]] = Map(
      "X" -> Rock[Hero],
      "Y" -> Paper[Hero],
      "Z" -> Scissors[Hero]
    )

    val resultMap = Map(
      "X" -> Lose,
      "Y" -> Draw,
      "Z" -> Win
    )
// TODO this gives and error, but it is a MatchError
    def playGame(
      opponentsMove: Move[Villain],
      myMove: Move[Hero]
    ): MyGameResult =
      (opponentsMove, myMove) match {
        case (_: Rock[Villain], _: Rock[Hero])         => Draw
        case (_: Rock[Villain], _: Paper[Hero])        => Win
        case (_: Rock[Villain], _: Scissors[Hero])     => Lose
        case (_: Paper[Villain], _: Rock[Hero])        => Lose
        case (_: Paper[Villain], _: Paper[Hero])       => Draw
        case (_: Paper[Villain], _: Scissors[Hero])    => Win
        case (_: Scissors[Villain], _: Rock[Hero])     => Win
        case (_: Scissors[Villain], _: Paper[Hero])    => Lose
        case (_: Scissors[Villain], _: Scissors[Hero]) => Draw
      }

    def scoreGame(myMove: Move[Hero], result: MyGameResult): Int =
      myMove.value + result.points

    def scoreFixedGame(
      oppMove: Move[Villain],
      result: MyGameResult
    ): Int = {
      val myMove = result match {
        case Win =>
          oppMove match {
            case _: Rock[Villain]     => Paper[Hero]
            case _: Paper[Villain]    => Scissors[Hero]
            case _: Scissors[Villain] => Rock[Hero]
          }
        case Lose =>
          oppMove match {
            case _: Rock[Villain]     => Scissors[Hero]
            case _: Paper[Villain]    => Rock[Hero]
            case _: Scissors[Villain] => Paper[Hero]
          }
        case Draw =>
          oppMove match {
            case _: Rock[Villain]     => Rock[Hero]
            case _: Paper[Villain]    => Paper[Hero]
            case _: Scissors[Villain] => Scissors[Hero]
          }
      }
      result.points + myMove.value
    }

  }

  trait RockPaperScissors {
    val fileSource: String
  }
  object RockPaperScissors {
// TODO gotta fix this object

// Part 1
    val myScores: Seq[Int] = for {
      (oppMove, myMove, _) <- allGameMoves
      result = MyGameResult.playGame(oppMove, myMove)
    } yield MyGameResult.scoreGame(myMove, result)

    // Part 2
    val myRiggedScores: Seq[Int] = for {
      (oppMove, _, result) <- allGameMoves
    } yield MyGameResult.scoreFixedGame(oppMove, result)

    /** Fs2 - Finally Tagless impl
      */

    def allGamesFS2[F[_]: Files]
      : Stream[F, (Move[Villain], Move[Hero], MyGameResult)] =
      inputStream.through(stringToGameMovesPipe)

    def myScoresTotalFS2[F[_]: Files]: Stream[F, Int] =
      allGamesFS2.through(movesToScoreTotalPipe)

    def myRiggedScoresTotalFS2[F[_]: Files]: Stream[F, Int] =
      allGamesFS2.through(movesToRiggedScoreTotalPipe)
  }

  def outputStream[F[_]: Monad, A, B](
    stream: Stream[F, A]
  ): Reader[Pipe[F, A, Unit], Stream[F, Unit]] =
    Reader(pipe => stream.through(pipe))

  def main(args: Array[String]): Unit = {
//    println(allGameMoves)
//    println(myScores.sum)
//    println(myRiggedScores.sum)

    val mm = Rock[Hero]
    val om = Paper[Villain]

    println(MyGameResult.playGame(om, mm))

    def program[F[_]: Console: Monad: Files: Concurrent]
      : Reader[Pipe[F, Int, Unit], Stream[F, Unit]] = for {
      s1 <- outputStream(myScoresTotalFS2[F])
      s2 <- outputStream(myRiggedScoresTotalFS2[F])
    } yield s1 ++ s2

    program[IO].run(toConsolePipe[IO]).compile.drain.unsafeRunSync()
  }
}
