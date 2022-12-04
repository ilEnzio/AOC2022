package puzzles

import scala.io.Source

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

trait Day02 {}

sealed trait Move {
  val value: Int
  val all = List(Rock, Paper, Scissor)
}
case object Rock extends Move {
  override val value: Int = 1
}
case object Paper extends Move {
  override val value: Int = 2
}
case object Scissor extends Move {
  override val value: Int = 3
}

sealed trait MyGameResult {
  val points: Int
}

object MyGameResult {
  val all = List(Win, Lose, Draw)

  def evaluate(game: (Move, Move)): MyGameResult =
    game match {
      case Win(x)  => x
      case Lose(x) => x
      case Draw(x) => x
    }

  val opponentMoveMap = Map(
    "A" -> Rock,
    "B" -> Paper,
    "C" -> Scissor
  )

  val myMoveMap = Map(
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissor
  )

  val resultMap = Map(
    "X" -> Lose(),
    "Y" -> Draw(),
    "Z" -> Win()
  )

  //  def playGame(opponentsMove: Move, myMove: Move): MyGameResult =
  //    (opponentsMove, myMove) match {
  //      // using a tuple here is dangerous - could leave out a case, also order could be wrong
  //      // can I solve this by making play game a case class?
  //      case (Rock, Rock)       => Draw
  //      case (Rock, Paper)      => Win
  //      case (Rock, Scissor)    => Lose
  //      case (Paper, Rock)      => Lose
  //      case (Paper, Paper)     => Draw
  //      case (Paper, Scissor)   => Win
  //      case (Scissor, Rock)    => Win
  //      case (Scissor, Paper)   => Lose
  //      case (Scissor, Scissor) => Draw
  //    }

  def scoreGame(myMove: Move, result: MyGameResult): Int =
    myMove.value + result.points

  def scoreFixedGame(oppMove: Move, result: MyGameResult): Int = {
    val myMove = result match {
      case _: Win =>
        oppMove match {
          case Rock    => Paper
          case Paper   => Scissor
          case Scissor => Rock
        }
      case _: Lose =>
        oppMove match {
          case Rock    => Scissor
          case Paper   => Rock
          case Scissor => Paper
        }
      case _: Draw => oppMove
    }
    result.points + myMove.value
  }

  final case class Win() extends MyGameResult {
    override val points: Int = 6

  }

  object Win {
    def unapply(game: (Move, Move)): Option[Win] =
      game match {
        case (Rock, Paper)    => Some(Win())
        case (Paper, Scissor) => Some(Win())
        case (Scissor, Rock)  => Some(Win())
        case (_, _)           => None
      }
  }

  final case class Lose() extends MyGameResult {
    override val points: Int = 0
  }

  object Lose {
    def unapply(game: (Move, Move)): Option[Lose] =
      game match {
        case (Rock, Scissor)  => Some(Lose())
        case (Paper, Rock)    => Some(Lose())
        case (Scissor, Paper) => Some(Lose())
        case (_, _)           => None
      }
  }

  final case class Draw() extends MyGameResult {
    override val points: Int = 3
  }

  object Draw {
    def unapply(game: (Move, Move)): Option[Draw] =
      if (game._1 == game._2) Some(Draw()) else None
  }
}

object Day02 {
// TODO gotta fix this object
  val allGameMoves: List[(Move, Move, MyGameResult)] = Source
    .fromFile("src/main/scala/inputs/day02")
    .getLines()
    .mkString("")
    .grouped(3)
    .toList
    .map {
      _.toList match {
        case h :: _ :: t =>
          (
            MyGameResult.opponentMoveMap(h.toString),
            MyGameResult.myMoveMap(t.head.toString),
            MyGameResult.resultMap(t.head.toString)
          )
      }
    }

// Part 1
  val myScores: Seq[Int] = for {
    (oppMove, myMove, _) <- allGameMoves
    result = MyGameResult.evaluate((oppMove, myMove))
  } yield MyGameResult.scoreGame(myMove, result)

  // Part 2
  val myRiggedScores: Seq[Int] = for {
    (oppMove, _, result) <- allGameMoves
  } yield MyGameResult.scoreFixedGame(oppMove, result)

  def main(args: Array[String]): Unit = {
    println(allGameMoves)
    println(myScores.sum)
    println(myRiggedScores.sum)
  }

}
