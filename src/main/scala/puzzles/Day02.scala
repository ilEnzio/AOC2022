package puzzles

import scala.io.Source

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
      case _: Draw =>
        oppMove match {
          case Rock    => Rock
          case Paper   => Paper
          case Scissor => Scissor
        }
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
      game match {
        case (Rock, Rock)       => Some(Draw())
        case (Paper, Paper)     => Some(Draw())
        case (Scissor, Scissor) => Some(Draw())
        case (_, _)             => None
      }
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

  val myScores: Seq[Int] = for {
    (oppMove, myMove, _) <- allGameMoves
    result = MyGameResult.evaluate((oppMove, myMove))
  } yield MyGameResult.scoreGame(myMove, result)

  val myRiggedScores: Seq[Int] = for {
    (oppMove, _, result) <- allGameMoves
  } yield MyGameResult.scoreFixedGame(oppMove, result)

  def main(args: Array[String]): Unit = {
    println(allGameMoves)
    println(myScores.sum)
    println(myRiggedScores.sum)
  }

}
