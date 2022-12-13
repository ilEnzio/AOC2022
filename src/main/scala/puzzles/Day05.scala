package puzzles

import puzzles.SupplyOps.{advMoveCrates, moveCrates}

import java.util.regex.Pattern.matches
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.matching.Regex

/** --- Day 5: Supply Stacks ---
  * The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.
  *
  * The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.
  *
  * The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
  *
  * They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:
  *
  *    [D]
  * [N] [C]
  * [Z] [M] [P]
  * 1   2   3
  *
  * move 1 from 2 to 1
  * move 3 from 1 to 3
  * move 2 from 2 to 1
  * move 1 from 1 to 2
  * In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.
  *
  * Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:
  *
  * [D]
  * [N] [C]
  * [Z] [M] [P]
  * 1   2   3
  * In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
  *
  *        [Z]
  *        [N]
  *    [C] [D]
  *    [M] [P]
  * 1   2   3
  * Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
  *
  *        [Z]
  *        [N]
  * [M]     [D]
  * [C]     [P]
  * 1   2   3
  * Finally, one crate is moved from stack 1 to stack 2:
  *
  *        [Z]
  *        [N]
  *        [D]
  * [C] [M] [P]
  * 1   2   3
  * The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
  *
  * After the rearrangement procedure completes, what crate ends up on top of each stack?
  */
trait Day05 {}
// case class Crates(value: char)
case class Stack(crates: List[String])                       {}
case class CraneOp(num: Int, fromKey: String, toKey: String) {}

object SupplyOps {

  def moveCrates(
    op: CraneOp,
    supplies: Map[String, Stack]
  ): Map[String, Stack] = {
    def doPop(n: Int, s1: Stack, s2: Stack): (Stack, Stack) =
      if (n <= 0) (s1, s2)
      else {
        // TODO how to make this exhaustive?
        (s1.crates, s2.crates) match {
          case (h :: t, x) => doPop(n - 1, Stack(t), Stack(h :: x))
        }
      }

    val (from, to) =
      doPop(op.num, supplies(op.fromKey), supplies(op.toKey))
    val fromU = supplies.updated(op.fromKey, from)
    val toU   = fromU.updated(op.toKey, to)
    toU
  }
  def advMoveCrates(
    op: CraneOp,
    supplies: Map[String, Stack]
  ): Map[String, Stack] = {
    def doPop(n: Int, s1: Stack, s2: Stack): (Stack, Stack) =
      if (n <= 0) (s1, s2)
      else {
        (s1.crates, s2.crates) match {
          case (x, y) => doPop(n - n, Stack(x.drop(n)), Stack(x.take(n) ++ y))
        }
      }

    val (from, to) =
      doPop(op.num, supplies(op.fromKey), supplies(op.toKey))
    val withUpdatedFromStack      = supplies.updated(op.fromKey, from)
    val withUpdateFromAndToStacks = withUpdatedFromStack.updated(op.toKey, to)
    withUpdateFromAndToStacks
  }

  def topCrates(supplies: Map[String, Stack]): List[Option[String]] =
    ListMap(supplies.toSeq.sortBy(_._1): _*).values
      .map(_.crates.headOption)
      .toList
}
object Day05 {

  // crates - Upper case letter
  // stacks - (link) list
  // 1 - number of head pops and attach to
  // 2nd - the list to pop
  // 3rd the list to push

  val supplies = Map(
    "1" -> Stack(List("M", "F", "C", "W", "T", "D", "L", "B")),
    "2" -> Stack(List("L", "B", "N")),
    "3" -> Stack(List("V", "L", "T", "H", "C", "J")),
    "4" -> Stack(List("W", "J", "P", "S")),
    "5" -> Stack(List("R", "L", "T", "F", "C", "S", "Z")),
    "6" -> Stack(List("Z", "N", "H", "B", "G", "D", "W")),
    "7" -> Stack(List("N", "C", "G", "V", "P", "S", "M", "F")),
    "8" -> Stack(List("Z", "C", "V", "F", "J", "R", "Q", "W")),
    "9" -> Stack(List("H", "L", "M", "P", "R"))
  )

  val pattern = """(\d+)""".r

  def decode(file: String) = {
    val s = Source
      .fromFile(file)
    val data = s
      .getLines()
      .toList
      .map(x => pattern.findAllIn(x).mkString(" "))
      .map { x =>
        val y = x.split(" ")
        CraneOp(y(0).toInt, y(1), y(2))
      }

    s.close()
    data
  }

  def main(args: Array[String]): Unit = {
    //    println(supplies)
    //    val op = CraneOp(5, "3", "6")
    //    println(SupplyOps.moveCrates(op))
    //    println(SupplyOps.topCrates(supplies))
    //    println(decode("src/main/scala/inputs/day05b"))
    val allOps =
      Day05.decode("src/main/scala/inputs/day05b")
    val moveAllCrates = allOps.foldLeft(Day05.supplies) { (s, v) =>
      moveCrates(v, s)
    }

    val advMoveAllCrates = allOps.foldLeft(Day05.supplies) { (s, v) =>
      advMoveCrates(v, s)
    }
    println(SupplyOps.topCrates(moveAllCrates))
    println(SupplyOps.topCrates(advMoveAllCrates))

  }
}
