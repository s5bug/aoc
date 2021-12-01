package tf.bug.aoc.y2020

import cats.effect._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day01 extends AOCApp(2020, 1) {

  def solve[F[_]: Async](input: Stream[F, String], entryCount: Int): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .compile
      .toVector
      .map { v =>
        v.combinations(entryCount).collectFirst {
          case v if v.sum == 2020 => v.product
        }.get.toString
      }

  override def part1[F[_]: Async](input: fs2.Stream[F, String]): F[String] =
    solve(input, 2)

  override def part2[F[_]: Async](input: fs2.Stream[F, String]): F[String] =
    solve(input, 3)

}
