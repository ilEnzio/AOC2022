package puzzles.Day03

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.Files
import fs2.{Pipe, Stream}
import puzzles.Day03.IOService.{
  inputStream,
  ruckSackToCommonCharSumPipe,
  ruckSackToCommonCharsPipe,
  ruckSackToConsolePipe,
  ruckSacktoStringPipe,
  stringToConsolePipe,
  stringToRuckSacksPipe
}
import cats._
import cats.data._
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.syntax.all._

import scala.io.Source

/** --- Day 3: Rucksack Reorganization ---
  * One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey. Unfortunately, that Elf didn't quite follow the packing instructions, and so a few items now need to be rearranged.
  *
  * Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the two compartments. The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.
  *
  * The Elves have made a list of all of the items currently in each rucksack (your puzzle input), but they need your help finding the errors. Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different types of items).
  *
  * The list of items for each rucksack is given as characters all on a single line. A given rucksack always has the same number of items in each of its two compartments, so the first half of the characters represent items in the first compartment, while the second half of the characters represent items in the second compartment.
  *
  * For example, suppose you have the following list of contents from six rucksacks:
  *
  * vJrwpWtwJgWrhcsFMMfFFhFp
  * jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
  * PmmdzqPrVvPwwTWBwg
  * wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
  * ttgJtRGJQctTZtZT
  * CrZsJsPPZsGzwwsLwLmpwMDw
  * The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means its first compartment contains the items vJrwpWtwJgWr, while the second compartment contains the items hcsFMMfFFhFp. The only item type that appears in both compartments is lowercase p.
  * The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. The only item type that appears in both compartments is uppercase L.
  * The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only common item type is uppercase P.
  * The fourth rucksack's compartments only share item type v.
  * The fifth rucksack's compartments only share item type t.
  * The sixth rucksack's compartments only share item type s.
  * To help prioritize item rearrangement, every item type can be converted to a priority:
  *
  * Lowercase item types a through z have priorities 1 through 26.
  * Uppercase item types A through Z have priorities 27 through 52.
  * In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.
  *
  * Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?
  *
  * Your puzzle answer was 7848.
  *
  * --- Part Two ---
  * As you finish identifying the misplaced items, the Elves come to you with another issue.
  *
  * For safety, the Elves are divided into groups of three. Every Elf carries a badge that identifies their group. For efficiency, within each group of three Elves, the badge is the only item type carried by all three Elves. That is, if a group's badge is item type B, then all three Elves will have item type B somewhere in their rucksack, and at most two of the Elves will be carrying any other item type.
  *
  * The problem is that someone forgot to put this year's updated authenticity sticker on the badges. All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.
  *
  * Additionally, nobody wrote down which item type corresponds to each group's badges. The only way to tell which item type is the right one is by finding the one item type that is common between all three Elves in each group.
  *
  * Every set of three lines in your list corresponds to a single group, but each group can have a different badge item type. So, in the above example, the first group's rucksacks are the first three lines:
  *
  * vJrwpWtwJgWrhcsFMMfFFhFp
  * jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
  * PmmdzqPrVvPwwTWBwg
  * And the second group's rucksacks are the next three lines:
  *
  * wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
  * ttgJtRGJQctTZtZT
  * CrZsJsPPZsGzwwsLwLmpwMDw
  * In the first group, the only item type that appears in all three rucksacks is lowercase r; this must be their badges. In the second group, their badge item type must be Z.
  *
  * Priorities for these items must still be found to organize the sticker attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.
  *
  * Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?
  */

trait Day03 {}

case class RuckSack(comp1: String, comp2: String) {
  def commonItemDistinct: String = comp1.filter(comp2.contains(_)).distinct
  def commonItem: String         = comp1.filter(comp2.contains(_))
  def combinedSack: String       = comp1 ++ comp2
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

  val allCommonItems: List[Char] = allRuckSacks.map(_.commonItemDistinct(0))

  val allElfGroups: List[List[String]] =
    allRuckSacks.map(_.combinedSack).grouped(3).toList

  def elfGroupCommonItems(group: List[String]) =
    group(0).filter(x => group(1).contains(x) && group(2).contains(x)).distinct

  def charToInt(char: Char) =
    char.isUpper match {
      case true  => char.toInt - 64 + 26
      case false => char.toInt - 96
    }

  val elfGroupItems = allElfGroups.map(elfGroupCommonItems(_)(0))

  /** Fs2 - Finally Tagless impl
    */

  def allRuckSacksFS2[F[_]: Files]: Stream[F, RuckSack] =
    inputStream.through(stringToRuckSacksPipe)

  def allRuckSacksAsStringsFS2[F[_]: Files]: Stream[F, String] =
    inputStream.through(stringToRuckSacksPipe).through(ruckSacktoStringPipe)

  def allCommonItemsFS2[F[_]: Files]: Stream[F, String] =
    allRuckSacksFS2.through(ruckSackToCommonCharsPipe)

  def allCommonItemsSumFS2[F[_]: Files]: Stream[F, String] =
    allRuckSacksFS2.through(ruckSackToCommonCharSumPipe)

  // P[F, A, B] => Stream[F, Unit]
  def outputStream[F[_]: Monad, A, B](
    stream: Stream[F, A]
  ): Reader[Pipe[F, A, Unit], Stream[F, Unit]] =
    Reader(pipe => stream.through(pipe))

  // Convert everything to string
  // then use dependency injection on the string.
  // so that means my toConsole pipe is: stringToConsolePipe

  def main(args: Array[String]): Unit = {
    //    println(allCommonItems)
    println(allCommonItems.map(charToInt(_)).sum) // 96, 4
//
    println(allElfGroups)
//    println(allElfGroups.map(elfGroupCommonItems(_)))
//    println(elfGroupItems.map(charToInt(_)).sum)

    //  println(allRuckSacksFS2[IO].compile.toList.unsafeRunSync())
    //  println(allCommonItemsFS2[IO].compile.toList.unsafeRunSync())

    def program[F[_]: Monad: Files: Console: Concurrent]
      : Reader[Pipe[F, String, Unit], Stream[F, Unit]] = for {
//      s1 <- outputStream(allRuckSacksAsStringsFS2[F])
      s2 <- outputStream(allCommonItemsSumFS2[F])
    } yield s2

    program[IO].run(stringToConsolePipe[IO]).compile.drain.unsafeRunSync()

  }
}
