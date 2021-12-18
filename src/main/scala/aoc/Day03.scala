package aoc

object Day03 extends Day(3) {
  def bit(values: List[String], pos: Int, mostCommon: Boolean): Char = {
    val (zeroes, ones) = values.map(_(pos)).foldLeft((0, 0)) {
      case ((z, o), '0') => (z + 1, o)
      case ((z, o), '1') => (z, o + 1)
      case _             => sys.error("String is not binary")
    }
    if (mostCommon) {
      if (ones >= zeroes) '1'
      else '0'
    } else {
      if (zeroes <= ones) '0'
      else '1'
    }
  }

  implicit class BinaryString(val value: String) extends AnyVal {
    def *(other: String): String =
      (Integer.parseInt(value, 2) * Integer.parseInt(other, 2)).toString
  }

  def partOne(): String = {
    val gamma =
      (0 until input.head.length).map(pos => bit(input, pos, true)).mkString
    val epsilon =
      (0 until input.head.length).map(pos => bit(input, pos, false)).mkString
    gamma * epsilon
  }
  def partTwo(): String = {
    def loop(left: List[String], pos: Int, mostCommon: Boolean): String =
      left match {
        case Nil         => sys.error("Empty list")
        case head :: Nil => head
        case _ =>
          val nextBit = bit(left, pos, mostCommon)
          val nextLeft = left.filter(_(pos) == nextBit)
          loop(nextLeft, pos + 1, mostCommon)
      }

    val oxy = loop(input, 0, mostCommon = true)
    val co2 = loop(input, 0, mostCommon = false)

    oxy * co2
  }
}
