package tf.bug.aoc.y2021

import cats.effect.Async
import cats.parse._
import cats.syntax.all._
import fs2._
import tf.bug.aoc.AOCApp

object Day05 extends AOCApp(2021, 5) {

  case class Point(x: Int, y: Int)
  case class Line(start: Point, end: Point) {
    def range: LazyList[Point] = {
      def go(here: Point): LazyList[Point] = {
        if(here == end) here #:: LazyList.empty
        else {
          val xDelta = if(here.x < end.x) 1 else if(here.x > end.x) -1 else 0
          val yDelta = if(here.y < end.y) 1 else if(here.y > end.y) -1 else 0
          here #:: go(Point(here.x + xDelta, here.y + yDelta))
        }
      }
      go(start)
    }
    def isHorizontal: Boolean = start.y == end.y
    def isVertical: Boolean = start.x == end.x
  }

  val natural: Parser[Int] = Parser.charsWhile(_.isDigit).map(_.toInt)

  val point: Parser[Point] = (natural, Parser.string(",") *> natural).mapN(Point)
  val line: Parser[Line] = (point, Parser.string(" -> ") *> point).mapN(Line)

  def solve[F[_]: Async](input: Stream[F, String], filter: Line => Boolean): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(line.parseAll)
      .collect { case Right(l) => l }
      .filter(filter)
      .compile
      .fold(Map[Point, Int]()) {
        (acc, line) =>
          // For every line in our input,
          line.range.foldLeft(acc) {
            (m, p) =>
              // Increment the map at every point in that line
              m.updatedWith(p) {
                case Some(n) => Some(n + 1)
                case None => Some(1)
              }
          }
      }
      .map(_.count(_._2 >= 2))
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, l => l.isHorizontal || l.isVertical)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, _ => true)
}
