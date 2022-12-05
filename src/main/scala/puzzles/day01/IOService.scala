package puzzles.day01

import scala.io.Source

object IOService extends Day01 {

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

}
