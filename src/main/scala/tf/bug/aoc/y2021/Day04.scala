package tf.bug.aoc.y2021

import cats.effect._
import cats.parse._
import cats.syntax.all._
import fs2._

import scala.collection.BitSet
import tf.bug.aoc.AOCApp

import scala.annotation.tailrec

object Day04 extends AOCApp(2021, 4) {

  case class BoardProgress(score: Int, steps: Int, marks: Int)
  case class BoardResult(totalScore: Int, steps: Int)

  def hasBingo(marks: Int): Boolean = {
    val rowMask: Int = (1 << 5) - 1
    lazy val anyRow = (0 to 4).exists { group =>
      val markBits = marks >> (5 * group)
      (markBits & rowMask) == rowMask
    }
    lazy val columnMask = (0 to 4).foldLeft(rowMask) { (result, group) =>
      val markBits = marks >> (5 * group)
      result & markBits
    }
    anyRow || (columnMask != 0)
  }

  case class Board(cells: Vector[Int]) {
    def result(calls: Vector[Int]): BoardResult = {
      @tailrec def go(progress: BoardProgress): BoardProgress = {
        if(progress.steps > calls.size) progress
        else if(hasBingo(progress.marks)) progress
        else {
          val call = calls(progress.steps)
          val callIdx = cells.indexOf(call)
          if(callIdx == -1) go(BoardProgress(progress.score, progress.steps + 1, progress.marks))
          else {
            val newScore = progress.score - call
            val mark = progress.marks | (1 << callIdx)
            go(BoardProgress(newScore, progress.steps + 1, mark))
          }
        }
      }

      val initialScore = cells.sum
      val result = go(BoardProgress(initialScore, 0, 0))
      BoardResult(result.score * calls(result.steps - 1), result.steps)
    }
  }

  trait Strategy {
    def select(x: BoardResult, y: BoardResult): BoardResult
    def exit(r: BoardResult, calls: Vector[Int]): Boolean
  }
  object Win extends Strategy {
    override def select(x: BoardResult, y: BoardResult): BoardResult =
      List(x, y).minBy(_.steps)
    override def exit(r: BoardResult, calls: Vector[Int]): Boolean =
      r.steps == 5
  }
  object Lose extends Strategy {
    override def select(x: BoardResult, y: BoardResult): BoardResult =
      List(x, y).maxBy(_.steps)
    override def exit(r: BoardResult, calls: Vector[Int]): Boolean =
      r.steps == calls.size
  }

  def solve[F[_]: Async](input: Stream[F, String], strategy: Strategy): F[String] =
    input
      .through(text.lines)
      .pull
      .uncons1
      .flatMap {
        case Some((callStr, rest)) =>
          val calls = callStr.split(",").map(_.toInt).toVector
          val boardVectors = rest.split(_.isEmpty).filter(_.nonEmpty).map { boards =>
            boards.toVector.flatMap(_.trim.split("\\s+").map(_.toInt))
          }
          val results = boardVectors.map(Board).map(_.result(calls))
          val winner = results.scan1(strategy.select).takeWhile(!strategy.exit(_, calls), takeFailure = true)
          Pull.eval(winner.compile.last).map(_.get).flatMap(Pull.output1)
      }.stream.compile.last.map(_.get.totalScore.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, Win)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, Lose)

}
