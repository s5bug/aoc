package tf.bug.aoc.y2021

import cats._
import cats.effect._
import cats.parse._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day02 extends AOCApp(2021, 2) {

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Forward extends Direction

  case class Command(direction: Direction, length: Int)

  case class Position(horiz: Int = 0, depth: Int = 0, aim: Int = 0)

  val natural: Parser[Int] = Parser.charsWhile(_.isDigit).map(_.toInt)

  val up: Parser[Up.type] = Parser.string("up").as(Up)
  val down: Parser[Down.type] = Parser.string("down").as(Down)
  val forward: Parser[Forward.type] = Parser.string("forward").as(Forward)
  val direction: Parser[Direction] = Parser.oneOf(List(up, down, forward))

  val command: Parser[Command] = (direction, Parser.string(" ") *> natural).mapN(Command)

  def solve[F[_]: Async](input: Stream[F, String], move: (Position, Command) => Position): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(command.parseAll)
      .collect { case Right(c) => c }
      .compile
      .fold(Position())(move)
      .map(p => p.horiz * p.depth)
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, (pos, c) => c match {
      case Command(Up, length) => pos.copy(depth = pos.depth - length)
      case Command(Down, length) => pos.copy(depth = pos.depth + length)
      case Command(Forward, length) => pos.copy(horiz = pos.horiz + length)
    })

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, (pos, c) => c match {
      case Command(Up, length) => pos.copy(aim = pos.aim - length)
      case Command(Down, length) => pos.copy(aim = pos.aim + length)
      case Command(Forward, length) => pos.copy(horiz = pos.horiz + length, depth = pos.depth + (length * pos.aim))
    })

}
