package tf.bug.aoc.y2021

import cats.effect._
import cats.parse._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day07 extends AOCApp(2021, 7) {

  def median(v: Vector[Int]): Int = {
    val sorted = v.sorted
    val medianIdx = sorted.length / 2
    val median = if(sorted.length % 2 == 0) {
      (sorted(medianIdx) + sorted(medianIdx + 1)) / 2
    } else sorted(medianIdx)

    median
  }

  def linearDistance(x: Int, y: Int): Int = Math.abs(x - y)

  def mean(v: Vector[Int]): Int = {
    v.sum / v.length
  }

  def triangularDistance(x: Int, y: Int): Int = {
    val l = linearDistance(x, y)
    (l * (l + 1)) / 2
  }

  def solve[F[_]: Async](input: Stream[F, String], optimal: Vector[Int] => Int, distance: (Int, Int) => Int): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(s => Stream.emits(s.split(",")))
      .map(_.toInt)
      .compile
      .toVector
      .map { v =>
        val opt = optimal(v)
        v.map(distance(opt, _)).sum
      }
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, median, linearDistance)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, mean, triangularDistance)

}
