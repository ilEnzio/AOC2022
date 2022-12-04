package day01_Data

import org.scalacheck.Gen
import org.scalacheck.Gen.choose
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
      snacks = for (_ <- 1 to numSnack) yield genSnack.sample.get
    } yield Elf(snacks.toList)
  }

  val genSnackData: Gen[IndexedSeq[IndexedSeq[Int]]] = {
    for {
      groups <- choose(2, 250)
      numSnacks     = choose(1, 15).sample.get
      snackValueNum = choose(1000, 99999).sample.get
      temp = for (_ <- 1 to groups)
        yield for (_ <- 1 to numSnacks) yield snackValueNum
    } yield temp
  }

  val genElfData: Gen[List[Int]] = {
    for {
      numSnack <- choose(1, 15)
      numValue <- choose(1000, 99999)
      snacksData = for (_ <- 1 to numSnack) yield numValue
    } yield snacksData.toList
  }

//  val genSnackData: Gen[List[List[Int]]] = {
//    for {
//      groupsNum <- choose(2, 250)
//      temp = for (_ <- 1 to groupsNum) yield genElfData.sample.get
//    } yield temp.toList
//  }

}
