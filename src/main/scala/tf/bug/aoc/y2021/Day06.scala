package tf.bug.aoc.y2021

import cats._
import cats.effect._
import cats.parse._
import cats.syntax.all._
import fs2._
import spire.math._
import tf.bug.aoc.AOCApp

/*
 * Much thanks and love to https://twitter.com/Daanniii6, who taught me about recurrence relations, taught me how to
 * solve them, and did most of the number crunching in numpy for me. This solution would not be possible without her.
 */
object Day06 extends AOCApp(2021, 6) {

  /*
   * Given the recurrence relation f(n) = f(n - 7) + f(n - 9), we have a solution
   *
   *   f(n) = [roots of `r^9 = r^2 + 1`].zipWithIndex.map(c_idx * r_idx^n)
   *
   * where c_1 through c_9 are constants.
   *
   * These constants can be determined by setting up a 9-dimensional matrix,
   *
   *   M[c_1, c_2, c_3, ..., c_9] = [f(0), f(1), f(2), ..., f(8)]
   *
   * where M looks like:
   *
   *   /1  1  1  ... 1 \
   *   |               |
   *   |r  r  r      r |
   *   | 1  2  3 ...  9|
   *   |               |
   *   | 2  2  2      2|
   *   |r  r  r      r |
   *   | 1  2  3 ...  9|
   *   |               |
   *   |:: :: :: *.  ::|
   *   |               |
   *   | 8  8  8      8|
   *   |r  r  r      r |
   *   \ 1  2  3 ...  9/
   *
   * The roots and coefficients here are approximations, but if Spire had a way to progressively approximate the complex
   * roots of polynomials, they would be generated that way.
   */
  val roots: Vector[Complex[Real]] = Vector(
    Complex(Real("-0.99613062205543878580016325940960086882114410400390625000"), Real("+0.4173118363357926074996839815867133438587188720703125")),
    Complex(Real("-0.99613062205543878580016325940960086882114410400390625000"), Real("-0.4173118363357926074996839815867133438587188720703125")),
    Complex(Real("1.09102447048075701374614254746120423078536987304687500000"), Real("+0")),
    Complex(Real("0.73407789846375282039048215665388852357864379882812500000"), Real("+0.74206512196218721300056131440214812755584716796875")),
    Complex(Real("0.73407789846375282039048215665388852357864379882812500000"), Real("-0.74206512196218721300056131440214812755584716796875")),
    Complex(Real("-0.37921398065481037864543623072677291929721832275390625000"), Real("+0.8928775460861679835744553201948292553424835205078125")),
    Complex(Real("-0.37921398065481037864543623072677291929721832275390625000"), Real("-0.8928775460861679835744553201948292553424835205078125")),
    Complex(Real("0.09575446900611984946127819284811266697943210601806640625"), Real("+0.87019871867210374372092474004602991044521331787109375")),
    Complex(Real("0.09575446900611984946127819284811266697943210601806640625"), Real("-0.87019871867210374372092474004602991044521331787109375"))
  )
  val coefficients: Vector[Complex[Real]] = Vector(
    Complex(Real("0.0320259577825860453081929790641879662871360778808593750000"), Real("-2.2275309625967812388047661897871876135468482971191406250000000000000000000000000000000000000000000000e-02")),
    Complex(Real("0.0320259577825862326583283845593541627749800682067871093750"), Real("+2.2275309625967999738183067393038072623312473297119140625000000000000000000000000000000000000000000000e-02")),
    Complex(Real("0.8231672701491546950691713391279336065053939819335937500000"), Real("+5.1189274346244600214882595917851286508664068711942952971671871864600689150393009185791015625000000000e-18")),
    Complex(Real("0.1191330519538716659067034697727649472653865814208984375000"), Real("-3.0416579555264087325605615319545904640108346939086914062500000000000000000000000000000000000000000000e-02")),
    Complex(Real("0.1191330519538716659067034697727649472653865814208984375000"), Real("+3.0416579555264052631136095783404016401618719100952148437500000000000000000000000000000000000000000000e-02")),
    Complex(Real("-0.0442313635690097656238961576491419691592454910278320312500"), Real("-7.3369976849389798023715059116511838510632514953613281250000000000000000000000000000000000000000000000e-02")),
    Complex(Real("-0.0442313635690097378683205420202284585684537887573242187500"), Real("+7.3369976849389811901502866930968593806028366088867187500000000000000000000000000000000000000000000000e-02")),
    Complex(Real("-0.0185112812420253659839719517776757129468023777008056640625"), Real("+1.3442512587596661122191221693356055766344070434570312500000000000000000000000000000000000000000000000e-01")),
    Complex(Real("-0.0185112812420254353729109908499594894237816333770751953125"), Real("-1.3442512587596658346633660130464704707264900207519531250000000000000000000000000000000000000000000000e-01"))
  )
  val pairs: Vector[(Complex[Real], Complex[Real])] = coefficients.zip(roots)
  def populationAt(day: Int): BigInt = {
    pairs.map { case (coeff, root) => coeff * root.pow(day) }.reduce(_ + _).real.round.toRational.toBigInt
  }

  def solve[F[_]: Async](input: Stream[F, String], day: Int): F[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(s => Stream.emits(s.split(",")))
      .map(_.toInt)
      .mapAccumulate(Map[Int, BigInt]()) { (memo, age) =>
        memo.get(day - age) match {
          case Some(res) => (memo, res)
          case None =>
            val res = populationAt(day - age + 6)
            (memo + ((day - age) -> res), res)
        }
      }
      .map(_._2)
      .compile
      .foldMonoid
      .map(_.toString)

  override def part1[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, 80)

  override def part2[F[_]: Async](input: Stream[F, String]): F[String] =
    solve(input, 256)

}
