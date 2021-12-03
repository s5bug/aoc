package tf.bug.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day03 extends AOCApp(2021, 3) {

  def solveEntries[F[_]]: Pipe[F, String, Vector[Int]] = _
    .through(text.lines)
    .filter(_.nonEmpty)
    .map(_.map {
      case '0' => -1
      case '1' => 1
    }.toVector)

  def foldCommons(commons: Vector[Int], entry: Vector[Int], op: (Int, Int) => Int): Vector[Int] =
    commons.padZipWith(entry)((x, y) => op(x.getOrElse(0), y.getOrElse(0)))

  def toBinary(offsets: Vector[Int]): Int =
    offsets.zipWithIndex.foldLeft(0) {
      case (n, (x, i)) if x > 0 => n | (1 << ((offsets.size - 1) - i))
      case (n, _) => n
    }

  sealed trait Sign {
    def check(entry: Vector[Int], commons: Vector[Int], index: Int): Boolean
  }
  case object Positive extends Sign {
    override def check(entry: Vector[Int], commons: Vector[Int], idx: Int): Boolean = {
      lazy val entryIsInMajority = entry(idx) * commons(idx) > 0
      lazy val commonsIsZeroAndEntryIsPositive = commons(idx) == 0 && entry(idx) > 0
      entryIsInMajority || commonsIsZeroAndEntryIsPositive
    }
  }
  case object Negative extends Sign {
    override def check(entry: Vector[Int], commons: Vector[Int], idx: Int): Boolean = {
      lazy val entryIsInMinority = entry(idx) * commons(idx) < 0
      lazy val commonsIsZeroAndEntryIsNegative = commons(idx) == 0 && entry(idx) < 0
      entryIsInMinority || commonsIsZeroAndEntryIsNegative
    }
  }

  def filterRatings(entries: Vector[Vector[Int]], initialCommons: Vector[Int], sign: Sign): Vector[Int] = {
    initialCommons.indices.foldLeft((entries, initialCommons)) {
      case ((results, commons), idx) =>
        if(results.size > 1) {
          val (toRemain, toSubtract) = results.partition(sign.check(_, commons, idx))
          val newCommons = toSubtract.foldLeft(commons)(foldCommons(_, _, _ - _))
          (toRemain, newCommons)
        } else (results, commons)
    }._1.head
  }

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    input
      .through(solveEntries)
      .compile
      .fold(Vector.empty[Int])(foldCommons(_, _, _ + _))
      .map(commons => (commons.size, toBinary(commons)))
      .map { case (size, result) =>
        val op = ~result & ((1 << size) - 1)
        result * op
      }
      .map(_.toString)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    input
      .through(solveEntries)
      .compile
      .toVector
      .map { entries =>
        val initialCommons = entries.foldLeft(Vector.empty[Int])(foldCommons(_, _, _ + _))
        val o2gr = toBinary(filterRatings(entries, initialCommons, Positive))
        val co2sr = toBinary(filterRatings(entries, initialCommons, Negative))
        o2gr * co2sr
      }
      .map(_.toString)

}
