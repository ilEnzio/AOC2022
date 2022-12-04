package puzzles

import java.io.{BufferedWriter, FileWriter}
import scala.io.{Source}

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
trait Day01 {

  def snackData: List[List[Int]]
  val fileSource: String

}

final case class Snack(value: Int)
final case class Elf(inv: List[Snack]) {
  def caloriesCarried: Int = inv.map(_.value).sum
}

// TODO Really what I need is a encoder/decoder...
object Day01Interpreter extends Day01 {

  override val fileSource: String = "src/main/scala/inputs/day01"

  def decode(file: String): List[Elf] = {
    val s = Source
      .fromFile(file)
    val snackData = s
      .getLines()
      .map(_.toIntOption)
      // must have missed a HOF here...
      .foldLeft(List(List.empty[Int])) { (s, v) =>
        v match {
          case Some(value) => (value :: s.head) :: s.tail
          case None        => List.empty[Int] :: s
        }
      }
    s.close()
    for {
      intList <- snackData
      snacks = intList.map(Snack)
      elf    = Elf(snacks)
    } yield elf
  }

  def encode(elves: List[Elf]): String = {
    val strings: List[String] = for {
      elf <- elves.reverse
      inv       = elf.inv.reverse.map(_.value)
      invString = inv.mkString("\n")
    } yield invString

    strings.mkString("\n\n")
  }

  def writeFile(filename: String, s: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(filename))
    for (line <- s)
      bw.write(line)
    bw.close()
  }

  override def snackData: List[List[Int]] = {
    val source = Source
      .fromFile(fileSource)

    val res = source
      .getLines()
      .map(_.toIntOption)
      // must have missed a HOF here...
      .foldLeft(List(List.empty[Int])) { (s, v) =>
        v match {
          case Some(value) => (value :: s.head) :: s.tail
          case None        => List.empty[Int] :: s
        }
      }
    source.close()
    res
  }

}

object Day01 {

  val allElvesInvSize: List[Int] = for {
    elf <- Day01Interpreter.snackData.map(_.map(Snack(_))).map(Elf(_))
  } yield elf.inv.length

  def allElvesSnackStatus(snackData: List[List[Int]]): List[Int] = for {
    intList <- snackData
    snacks = intList.map(Snack)
    elf    = Elf(snacks)
  } yield elf.caloriesCarried

  def elfWithMostSnacks(snackData: List[List[Int]]) =
    allElvesSnackStatus(snackData).max

  def top3ElvesByCalories(snackData: List[List[Int]]) =
    allElvesSnackStatus(snackData).sorted.takeRight(3)

  def main(args: Array[String]): Unit = {
    println(allElvesSnackStatus(Day01Interpreter.snackData).size)
    println(allElvesSnackStatus(Day01Interpreter.snackData).max)
    println(top3ElvesByCalories(Day01Interpreter.snackData).sum)
    println(allElvesInvSize.min)

  }

}
