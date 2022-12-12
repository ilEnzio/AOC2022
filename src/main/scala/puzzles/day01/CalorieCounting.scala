package puzzles.day01

import cats._
import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all._
import cats.effect.unsafe.implicits.global
import fs2.io.file.Files
import fs2.Stream
import puzzles.day01.IOService._
import puzzles.day01.IOService.ElfCodec

/** --- Day 1: Calorie Counting ---
  *  Santa's reindeer typically eat regular reindeer food, but they need a lot of magical energy to deliver presents on Christmas. For that, their favorite snack is a special type of star fruit that only grows deep in the jungle. The Elves have brought you on their annual expedition to the grove where the fruit grows.
  *
  *  To supply enough magical energy, the expedition needs to retrieve a minimum of fifty stars by December 25th. Although the Elves assure you that the grove has plenty of fruit, you decide to grab any fruit you see along the way, just in case.
  *
  *  Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
  *
  *  The jungle must be too overgrown and difficult to navigate in vehicles or access from the air; the Elves' expedition traditionally goes on foot. As your boats approach land, the Elves begin taking inventory of their supplies. One important consideration is food - in particular, the number of Calories each Elf is carrying (your puzzle input).
  *
  *  The Elves take turns writing down the number of Calories contained by the various meals, snacks, rations, etc. that they've brought with them, one item per line. Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.
  *
  *  For example, suppose the Elves finish writing their items' Calories and end up with the following list:
  *
  *  1000
  *  2000
  *  3000
  *
  *  4000
  *
  *  5000
  *  6000
  *
  *  7000
  *  8000
  *  9000
  *
  *  10000
  *  This list represents the Calories of the food carried by five Elves:
  *
  *  The first Elf is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
  *  The second Elf is carrying one food item with 4000 Calories.
  *  The third Elf is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
  *  The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
  *  The fifth Elf is carrying one food item with 10000 Calories.
  *  In case the Elves get hungry and need extra snacks, they need to know which Elf to ask: they'd like to know how many Calories are being carried by the Elf carrying the most Calories. In the example above, this is 24000 (carried by the fourth Elf).
  *
  * Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
  *
  * --- Part Two ---
  * By the time you calculate the answer to the Elves' question, they've already realized that the Elf carrying the most Calories of food might eventually run out of snacks.
  *
  * To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.
  *
  * In the example above, the top three Elves are the fourth Elf (with 24000 Calories), then the third Elf (with 11000 Calories), then the fifth Elf (with 10000 Calories). The sum of the Calories carried by these three elves is 45000.
  *
  * Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
  */
object Day01 {
  trait CalorieCounting {
    val fileSource: String
  }

  final case class Snack(value: Int)

  final case class Elf(inv: List[Snack]) {
    def caloriesCarried: Int = inv.map(_.value).sum
  }

  trait Day01Codec[A] {
    def encode(ag: A): String

    def decode(file: String): A
  }

  object CalorieCounting {

    val allElves: List[Elf] =
      IOService.decode(fileSource)

    def allElvesInvSize(allElves: List[Elf]): List[Int] =
      allElves.map(_.inv.length)

    def allElvesSnackStatus(elves: List[Elf]): List[Int] =
      elves.map(_.caloriesCarried)

    def elfWithMostSnacks(elves: List[Elf]): Int =
      elves.map(_.caloriesCarried).max

    def top3ElvesByCalories(elves: List[Elf]): List[Int] =
      allElvesSnackStatus(elves).sorted.takeRight(3)

    /** Fs2 - Finally Tagless implementation */
    def allElvesFS2[F[_]: Files]: Stream[F, Elf] =
      inputStream.through(stringToElfPipe)

    // TODO something is wrong with this
    def streamPrintedFS2[F[_]: Files: Functor: Console]: Stream[F, Unit] =
      for {
        _ <- allElvesFS2.through(toConsolePipe)
      } yield ()

    def elfWithMostSnacksFS2[F[_]: Files: Functor: Console]: Stream[F, Elf] =
      allElvesFS2
        .through(mostSnacksPipe)

    def top3ElvesByCaloriesFS2[F[_]: Files]: Stream[F, Int] = for {
      elves <- allElvesFS2.through(top3CaloriesPipe)
    } yield elves.map(_.caloriesCarried).sum

  }

  def main(args: Array[String]): Unit = {
    import CalorieCounting._
    def program[F[_]: Console: Monad]: F[Unit] = for {
//      _ <- Console[F].println(allElvesFS2[IO].compile.toList.unsafeRunSync())
      _ <- Console[F].println(allElves.size)
      _ <- Console[F].println(elfWithMostSnacks(allElves))
      _ <- Console[F].println(top3ElvesByCalories(allElves).sum)
      _ <- Console[F].println(allElvesFS2[IO].compile.count.unsafeRunSync())
      _ <- Console[F].println(
        elfWithMostSnacksFS2[IO].compile.last.unsafeRunSync()
      )
      _ <- Console[F].println(
        top3ElvesByCaloriesFS2[IO].compile.last.unsafeRunSync()
      )
    } yield ()

//    allElvesFS2[IO]
//      .foreach(Console[IO].println(_))
//      .compile
//      .drain
//      .unsafeRunSync()

    program[IO].unsafeRunSync()

  }

}
