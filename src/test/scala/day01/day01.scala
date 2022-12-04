package day01

import day01_Data.Day01Generators.{genElfData, genElfGroup, genSnackData}
import org.scalacheck.Prop.{forAll, AnyOperators}
import org.scalacheck.Properties
import puzzles.{Day01, Elf}
import puzzles.Day01Interpreter.{decode, encode, fileSource, writeFile}

object day01Test extends Properties("Day 1 Tests") {

//   Snack test
//   Elf Tests

  object Day01TestInterpreter extends Day01 {
    override def snackData = genSnackData.sample.get

    override val fileSource: String = "src/main/scala/inputs/day01_encTest"
  }

  property(
    "the maximum calorie count is the number of elves times max snack value"
  ) = forAll(genSnackData) { snackData =>
    val allElvesSnackStatus: Seq[Int] = Day01.allElvesSnackStatus(snackData)
    val numElves                      = allElvesSnackStatus.size
    allElvesSnackStatus.sum < numElves * 15 * 99999
  }

  property("encode/decode round trip - Elves == Elves -> File -> Elves") =
    forAll(genElfGroup) { elves =>
      val encodedElves = encode(elves)
      writeFile(Day01TestInterpreter.fileSource, encodedElves)
      val decodedElvesFromFile = decode(Day01TestInterpreter.fileSource)
      elves ?= decodedElvesFromFile
    }
  // a the elf w/ the most calories is always positive

}
