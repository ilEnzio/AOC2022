package puzzles.day01

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source

// TODO soo... this code is not RF... it bunch of side effects
// TODO also if I put all the IOServices in one file I can flatten
// the puzzle package.  But the IOService will become massive and there will be conflicts
// if collaborators were working on different Days.

object IOService extends CalorieCounting {

  override val fileSource: String = "src/main/scala/inputs/day01"

  implicit object ElfCodec extends Day01Codec[List[Elf]] {
    def encode(
      elves: List[Elf]
    ): String = {
      val strings: List[String] = for {
        elf <- elves.reverse
        inv       = elf.inv.reverse.map(_.value)
        invString = inv.mkString("\n")
      } yield invString

      strings.mkString("\n\n")
    }

    def decode(
      file: String
    ): List[Elf] = {
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

  def writeFile(filename: String, s: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(filename))
    for (line <- s)
      bw.write(line)
    bw.close()
  }

}
