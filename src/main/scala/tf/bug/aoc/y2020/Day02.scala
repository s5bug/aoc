package tf.bug.aoc.y2020

import cats.effect.Async
import cats.parse.Parser
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day02 extends AOCApp(2020, 2) {

  case class Policy(least: Int, most: Int, target: Char)
  case class Entry(policy: Policy, password: String)

  val natural: Parser[Int] = Parser.charsWhile(_.isDigit).map(_.toInt)
  val policy: Parser[Policy] = (natural, Parser.char('-') *> natural, Parser.char(' ') *> Parser.anyChar).mapN(Policy)
  val entry: Parser[Entry] = (policy, Parser.string(": ") *> Parser.anyChar.rep.string).mapN(Entry)

  def solve[F[_]: Async](input: Stream[F, String], validation: Entry => Boolean): F[String] =
    input
      .through(text.lines)
      .map(entry.parse)
      .collect { case Right((_, entry)) => entry }
      .filter(validation)
      .compile
      .count
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, {
      case Entry(Policy(l, m, t), s) =>
        val cnt = s.count(_ == t)
        l <= cnt && cnt <= m
    })

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, {
      case Entry(Policy(f, l, t), s) =>
        (s(f - 1) == t) ^ (s(l - 1) == t)
    })

}
