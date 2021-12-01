package aoc

object Day01 extends InputReader(1) with Day {
  val ints = input.map(_.toInt)

  def slidingIncrease(windowSize: Int): Int = {
    ints.sliding(windowSize + 1).foldLeft(0) {
      case (i, window) => if (window.head < window.last) i + 1 else i
    }
  }

  override def partOne(): String = slidingIncrease(1).toString

  override def partTwo(): String = slidingIncrease(3).toString
}
