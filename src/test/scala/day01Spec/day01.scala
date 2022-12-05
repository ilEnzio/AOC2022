package day01Spec

import day01_Data.Day01Generators.genElfGroup
import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalacheck.Properties
import puzzles.day01.Day01Codec
import puzzles.day01.{Day01, Elf, Snack}

import scala.io.Source
import puzzles.day01.Day01.{elfWithMostSnacks, writeFile}

object day01Test extends Properties("Day 1 Tests") {

  object Day01_TestIOService extends Day01 {

    override val fileSource: String = "src/main/scala/inputs/day01_encTest"

    implicit object ElfCodec extends Day01Codec[List[Elf]] {
      override def encode(ag: List[Elf]): String = {
        val strings: List[String] = for {
          elf <- ag.reverse
          inv       = elf.inv.reverse.map(_.value)
          invString = inv.mkString("\n")
        } yield invString

        strings.mkString("\n\n")
      }

      override def decode(file: String): List[Elf] = {
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
    }
    def encode[A](elves: A)(implicit codec: Day01Codec[A]): String =
      codec.encode(elves)

    def decode[A](file: String)(implicit codec: Day01Codec[A]): A =
      codec.decode(file)
  }

  property(
    "the maximum calorie count is the number of elves times max snack value"
  ) = forAll(genElfGroup) { allElves =>
    val allElvesSnackStatus: Seq[Int] = Day01.allElvesSnackStatus(allElves)
    val numElves                      = allElvesSnackStatus.size
    allElvesSnackStatus.sum < numElves * 15 * 99999
  }

  import Day01_TestIOService._
  property("encode/decode round trip - Elves == Elves -> File -> Elves") =
    forAll(genElfGroup) { elves =>
      val encodedElves = encode(elves)
      writeFile(Day01_TestIOService.fileSource, encodedElves)
      val decodedElvesFromFile = decode(Day01_TestIOService.fileSource)
      elves ?= decodedElvesFromFile
    }

  property("elf with the most snacks is always positive") =
    forAll(genElfGroup) { elves =>
      elfWithMostSnacks(elves) > 0
    }
  // an the elf w/ the most calories is always positive

}
