package aoc

/**
 *  0
 * 1 2
 *  3
 * 4 5
 *  6
 */
object Day08 extends InputReader(8) with Day {
  val unique = Set(2, 3, 4, 7)
  override def partOne(): String = {
    input
      .flatMap(_.dropWhile(_ != '|').drop(2).split(" ").toList)
      .filter(s => unique(s.length))
      .length
      .toString
  }

  def deduce(signals: List[Set[Char]]): Map[Char, Int] = {
    val one = signals.find(_.size == 2).get
    val four = signals.find(_.size == 4).get
    val seven = signals.find(_.size == 3).get
    val eight = signals.find(_.size == 7).get

    val top = seven.diff(one).head

    val topRight = one.find(c => signals.filter(_(c)).length == 8).get
    val bottomRight = (one - topRight).head

    val middle = four.diff(one).find(c => signals.filter(_(c)).length == 7).get
    val topLeft = (four.diff(one) - middle).head

    val bottomLeft = eight.find(c => signals.filter(_(c)).length == 4).get

    val allButBottom = List(top, topLeft, topRight, middle, bottomLeft, bottomRight)

    val bottom = eight.diff(allButBottom.toSet).head

    (allButBottom ++ List(bottom)).zipWithIndex.toMap
  }

  val numbers = Map(
     "012456" -> 0,
     "25" -> 1,
     "02346" -> 2,
     "02356" -> 3,
     "1235" -> 4,
     "01356" -> 5,
     "013456" -> 6,
     "025" -> 7,
     "0123456" -> 8,
     "012356" -> 9,
  )

  override def partTwo(): String =
    input.map { line =>
      val sigs = line.takeWhile(_ != '|').trim.split(" ").toList.map(_.toSet)
      val nums = line.dropWhile(_ != '|').drop(2).split(" ").toList

      val mapping = deduce(sigs)

      nums.map(num => numbers(num.map(c => mapping(c)).sorted.mkString)).mkString("").toInt
    }.sum.toString
}
