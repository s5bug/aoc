package tf.bug.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day01 extends AOCApp(2021, 1) {

  def solve[F[_]: Async](input: Stream[F, String], groupSize: Int): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .sliding(groupSize + 1)
      .filter(c => c.last > c.head)
      .compile
      .count
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve[F](input, 1)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve[F](input, 3)

}
