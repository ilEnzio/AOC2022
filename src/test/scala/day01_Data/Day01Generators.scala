package day01_Data

import org.scalacheck.Gen
import org.scalacheck.Gen.{choose, listOf, listOfN}
import puzzles.{Elf, Snack}

object Day01Generators {

  val genSnack: Gen[Snack] = {
    for {
      num <- choose(1000, 99999)
    } yield Snack(num)
  }

  val genElf: Gen[Elf] = {
    for {
      numSnack <- choose(1, 15)
      snacks   <- listOfN(numSnack, genSnack)

    } yield Elf(snacks)
  }

  val genElfGroup: Gen[List[Elf]] = {
    for {
      num   <- choose(3, 250)
      elves <- listOfN(num, genElf)
    } yield elves
  }

//  val genSnackData: Gen[IndexedSeq[IndexedSeq[Int]]] = {
//    for {
//      groups <- choose(2, 250)
//      numSnacks     = choose(1, 15).sample.get
//      snackValueNum = choose(1000, 99999).sample.get
//      temp = for (_ <- 1 to groups)
//        yield for (_ <- 1 to numSnacks) yield snackValueNum
//    } yield temp
//  }

  val genElfData: Gen[List[Int]] = {
    for {
      numSnack <- choose(1, 15)
      genNumValue = choose(1000, 99999)
      snacksData <- listOfN(numSnack, genNumValue)
    } yield snacksData
  }

  val genSnackData: Gen[List[List[Int]]] = {
    for {
      groupsNum <- choose(2, 250) // number of elves
      snackData <- listOfN(groupsNum, genElfData)
    } yield snackData
  }

}
