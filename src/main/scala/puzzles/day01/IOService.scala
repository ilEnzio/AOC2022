package puzzles.day01

import cats.{Functor}
import fs2.io.file.{Files, Path}
import fs2.{Pipe, Stream}
import puzzles.day01.Day01.{CalorieCounting, Day01Codec, Elf, Snack}

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source
import cats.effect.std.Console

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

  /** FS2 version / Finally Tagless version
    */
  def inputStream[F[_]: Files]: Stream[F, String] =
    Files[F]
      .readUtf8Lines(Path(fileSource))

  // pipe = Stream[F, I] => Stream[F, O]
  def stringToElfPipe[F[_]]: Pipe[F, String, Elf] = inStream =>
    for {
      snackD <- inStream
        .fold(List(List.empty[Int])) { (s, v) =>
          v.toIntOption match {
            case Some(value) => (value :: s.head) :: s.tail
            case None        => List.empty[Int] :: s
          }
        }
        .map(sn => sn.map(snacks => Elf(snacks.map(Snack(_)))))

      elves <- Stream.emits(snackD.reverse)
    } yield elves

  def mostSnacksPipe[F[_]]: Pipe[F, Elf, Elf] = inStream =>
    inStream
      .fold(Elf(Nil)) { (s: Elf, v: Elf) =>
        if (v.caloriesCarried > s.caloriesCarried) v else s
      }
      .map(x => Elf(List(Snack(x.caloriesCarried))))

  def top3CaloriesPipe[F[_]]: Pipe[F, Elf, List[Elf]] = inStream =>
    inStream.fold(List.empty[Elf]) { (s, v) =>
      val sorted = s.sortBy(_.caloriesCarried)
      sorted.length < 3 match {
        case true => v :: s
        case _    => (v :: s).sortBy(_.caloriesCarried).drop(1)
      }
    }

  // TODO  can't get this to work
  def toConsolePipe[F[_]: Console: Functor]: Pipe[F, Elf, Unit] = inStream =>
    inStream.evalMap(x => Console[F].println(x.copy(inv = x.inv.reverse)))

}
