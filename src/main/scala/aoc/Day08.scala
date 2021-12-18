package aoc

/** 0 1 2 3 4 5 6
  */
object Day08 extends Day(8) {
  val splitInput = input.map { line =>
    (
      line.takeWhile(_ != '|').trim.split(" ").toList.map(_.toSet),
      line.dropWhile(_ != '|').drop(2).split(" ").toList
    )
  }

  val numbers =
    List(
      "012456",
      "25",
      "02346",
      "02356",
      "1235",
      "01356",
      "013456",
      "025",
      "0123456",
      "012356"
    )
      .map(_.map(_.toString.toInt).toSet)
      .zipWithIndex
      .toMap

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

    val allButBottom =
      List(top, topLeft, topRight, middle, bottomLeft, bottomRight)

    val bottom = eight.diff(allButBottom.toSet).head

    (allButBottom ++ List(bottom)).zipWithIndex.toMap
  }

  override def partOne(): String = {
    val unique = numbers.keys.toList
      .map(_.size)
      .groupBy(identity)
      .filter(_._2.size == 1)
      .keySet
    input
      .flatMap(_.dropWhile(_ != '|').drop(2).split(" ").toList)
      .filter(s => unique(s.length))
      .length
      .toString
  }

  override def partTwo(): String =
    splitInput
      .map { case (sigs, nums) =>
        val mapping = deduce(sigs)
        nums
          .map(_.map(c => mapping(c)).toSet)
          .map(set => numbers(set))
          .mkString
          .toInt
      }
      .sum
      .toString
}
