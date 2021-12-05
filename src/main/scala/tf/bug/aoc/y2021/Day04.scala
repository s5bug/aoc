package tf.bug.aoc.y2021

import cats.effect._
import cats.parse._
import cats.syntax.all._
import fs2._

import scala.collection.BitSet
import tf.bug.aoc.AOCApp

import scala.annotation.tailrec

object Day04 extends AOCApp(2021, 4) {

  case class BoardResult(totalScore: Int, steps: Int)

  /*
   * Based on the numbers that get called, we can generate a look-up-table, such that any board can hold the turns it's
   * called on instead of having to indexOf the Vector repeatedly (i.e. figuring out what turn the board wins on goes
   * from O(boardSize * numberOfCalls) to just O(boardSize)).
   */
  def generateLut(calls: Vector[Int]): Array[Int] = {
    val result: Array[Int] = new Array(_length = 100)
    calls.zipWithIndex.foreach { case (call, idx) => result(call) = idx }
    result
  }

  case class Board(cells: Vector[Int]) {
    /*
     * The turn a column or row wins is the maximum of the turn numbers it holds.
     * Then, for a board to win, take the minimum of all the column and row turns.
     */
    def winningTurn(lut: Array[Int]): Int = {
      val indices = cells.map(lut)
      (0 to 4).map { major =>
        val row = ((major * 5) until ((major + 1) * 5) by 1).map(indices)
        val col = (major until (major + 25) by 5).map(indices)
        Math.min(row.max, col.max)
      }.min
    }

    /*
     * We don't calculate the score automatically, but it can be quickly calculated in O(1) for any specific turn.
     */
    def scoreOnTurn(calls: Vector[Int], lut: Array[Int], turn: Int): Int = {
      cells.filter(lut(_) > turn).sum * calls(turn)
    }

    def result(calls: Vector[Int], lut: Array[Int]): BoardResult = {
      val steps = winningTurn(lut)
      BoardResult(scoreOnTurn(calls, lut, steps), steps)
    }
  }

  trait Strategy {
    def select(calls: Vector[Int], lut: Array[Int], x: BoardResult, y: Board): BoardResult
    def exit(r: BoardResult, calls: Vector[Int]): Boolean
  }
  object Win extends Strategy {
    override def select(calls: Vector[Int], lut: Array[Int], x: BoardResult, y: Board): BoardResult = {
      val ySteps = y.winningTurn(lut)
      if(ySteps < x.steps) BoardResult(y.scoreOnTurn(calls, lut, ySteps), ySteps)
      else x
    }
    override def exit(r: BoardResult, calls: Vector[Int]): Boolean =
      r.steps == 5
  }
  object Lose extends Strategy {
    override def select(calls: Vector[Int], lut: Array[Int], x: BoardResult, y: Board): BoardResult = {
      val ySteps = y.winningTurn(lut)
      if(ySteps > x.steps) BoardResult(y.scoreOnTurn(calls, lut, ySteps), ySteps)
      else x
    }
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
          val lookUpTable: Array[Int] = generateLut(calls)

          val boardVectors = rest.split(_.isEmpty).filter(_.nonEmpty).map { boards =>
            boards.toVector.flatMap(_.trim.split("\\s+").map(_.toInt))
          }

          val results = boardVectors.map(Board)
          results.pull.uncons1.flatMap {
            /*
             * The first board doesn't have to be selected against, but scan1 won't work as we don't want to convert
             * every single Board into a BoardResult (we don't want to calculate the score for a board unless it's a
             * selected board, which is what the Strategies do).
             */
            case Some((firstBoard, rest)) =>
              val initial = firstBoard.result(calls, lookUpTable)
              val selected = rest.scan(initial)(strategy.select(calls, lookUpTable, _, _))
              val earlyExit = selected.takeWhile(!strategy.exit(_, calls), takeFailure = true)
              Pull.eval(earlyExit.compile.last.map(_.get)).flatMap(Pull.output1)
          }
      }.stream.compile.last.map(_.get.totalScore.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, Win)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, Lose)

}
