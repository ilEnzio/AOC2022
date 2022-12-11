package day01Spec

import scala.io.Source
import day01_Data.Day01Generators.genElfGroup
import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalacheck.Properties
import puzzles.day01.Day01Codec
import puzzles.day01.{CalorieCounting, Elf, Snack}
import puzzles.day01.CalorieCounting.elfWithMostSnacks
import puzzles.day01.IOService.ElfCodec.{decode, encode}
import puzzles.day01.IOService.writeFile

object CalorieCountingSpec extends Properties("CalorieCounting Tests") {

  object Day01_TestIOService extends CalorieCounting {

    override val fileSource: String = "src/main/scala/inputs/day01_encTest"
  }

  property(
    "the maximum calorie count is the number of elves times max snack value"
  ) = forAll(genElfGroup) { allElves =>
    val allElvesSnackStatus: Seq[Int] =
      CalorieCounting.allElvesSnackStatus(allElves)
    val numElves = allElvesSnackStatus.size
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
