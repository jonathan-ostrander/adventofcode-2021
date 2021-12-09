package aoc

object Day09 extends InputReader(9) with Day {
  val map = input.zipWithIndex.flatMap {
    case (line, x) => line.zipWithIndex.map {
      case (c, y) => (x, y) -> c.toString.toInt
    }
  }.toMap

  def adjacents(p: (Int, Int)): List[(Int, Int)] =
    List((-1, 0), (1, 0), (0, -1), (0, 1)).map(a => (p._1 + a._1, p._2 + a._2))

  val lowPoints = map.filter { case ((x, y), v) =>
    adjacents((x, y)).forall { p =>
      map.getOrElse(p, 9) > v
    }
  }

  override def partOne(): String =
    lowPoints.map(_._2 + 1).sum.toString
  override def partTwo(): String = {
    def loop(toCheck: List[(Int, Int)], members: Set[(Int, Int)]): Int =
      toCheck match {
        case Nil => members.size
        case head :: tail => loop(
          tail ++ adjacents(head).filterNot(p => members(p) || map.getOrElse(p, 9) == 9),
          members + head,
        )
      }
    lowPoints.toList
      .map(p => loop(p._1 :: Nil, Set.empty))
      .sortBy(-1*_)
      .take(3)
      .product
      .toString
  }
}
