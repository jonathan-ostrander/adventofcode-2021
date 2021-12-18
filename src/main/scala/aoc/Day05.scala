package aoc

object Day05 extends Day(5) {
  def genPoints(from: Int, to: Int, diff: Int): List[Int] =
    if (from == to) List.fill(diff.abs.toInt + 1)(from)
    else if (from > to) (from to to by -1).toList
    else (from to to).toList

  def solve(includeDiagonals: Boolean): String =
    input
      .map { line =>
        val x1 :: y1 :: x2 :: y2 :: Nil =
          line.split(" -> ").toList.flatMap(_.split(",").map(_.toInt).toList)
        ((x1, y1), (x2, y2))
      }
      .filter { case ((x1, y1), (x2, y2)) =>
        includeDiagonals || x1 == x2 || y1 == y2
      }
      .flatMap { case ((x1, y1), (x2, y2)) =>
        genPoints(x1, x2, y1 - y2)
          .zip(genPoints(y1, y2, x1 - x2))
      }
      .groupBy(identity)
      .count(_._2.length >= 2)
      .toString

  override def partOne(): String = solve(false)

  override def partTwo(): String = solve(true)
}
