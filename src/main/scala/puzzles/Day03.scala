package puzzles

import scala.io.Source

trait Day03 {}

case class RuckSack(comp1: String, comp2: String) {
  def commonItemDistinct = comp1.filter(comp2.contains(_)).distinct
  def commonItem         = comp1.filter(comp2.contains(_))
  def combinedSack       = comp1 ++ comp2
}

object Day03 {

  val allRuckSacks: List[RuckSack] = Source
    .fromFile("src/main/scala/inputs/day03")
    .getLines()
    .toList
    .map { x =>
      val sack = x.splitAt(x.length / 2)
      RuckSack(sack._1, sack._2)
    }
//    .mkString("")

  val elfGroups: List[List[String]] =
    allRuckSacks.map(_.combinedSack).grouped(3).toList

  val commonItems: List[Char] = allRuckSacks.map(_.commonItemDistinct(0))

  def groupCommonItems(group: List[String]) =
    group(0).filter(x => group(1).contains(x) && group(2).contains(x)).distinct

  def charToInt(char: Char) =
    char.isUpper match {
      case true  => char.toInt - 64 + 26
      case false => char.toInt - 96
    }

  val elfGroupItems = elfGroups.map(groupCommonItems(_)(0))

  def main(args: Array[String]): Unit = {
//    println(allRuckSacks)
    println(commonItems)
    println(commonItems.map(charToInt(_)).sum) // 96, 4

    println(elfGroups)
    println(elfGroups.map(groupCommonItems(_)))
    println(elfGroupItems.map(charToInt(_)).sum)

  }

}
