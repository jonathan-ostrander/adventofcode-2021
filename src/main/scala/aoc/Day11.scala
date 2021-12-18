package aoc

object Day11 extends Day(11) {
  val init = input.zipWithIndex.flatMap { case (line, x) =>
    line.zipWithIndex.map { case (c, y) =>
      ((x, y), c.toString.toInt)
    }
  }.toMap

  val minX = init.map(_._1._1).min
  val maxX = init.map(_._1._1).max
  val minY = init.map(_._1._2).min
  val maxY = init.map(_._1._2).max

  def adjacent(point: (Int, Int)): List[(Int, Int)] =
    (-1 to 1)
      .flatMap(x => (-1 to 1).map(y => (point._1 + x, point._2 + y)))
      .filter(_ != point)
      .filter { case (x, y) =>
        minX <= x && x <= maxX && minY <= y && y <= maxY
      }
      .toList

  implicit class Octopuses(value: Map[(Int, Int), Int]) {
    def next: Map[(Int, Int), Int] = {
      val bumped = value.transform { case (_, i) => i + 1 }
      val initialLit = bumped.filter(_._2 > 9).keys.toList

      def loop(
          visited: Set[(Int, Int)],
          litLeft: List[(Int, Int)],
          current: Map[(Int, Int), Int]
      ): Map[(Int, Int), Int] = litLeft match {
        case Nil => current
        case head :: tail =>
          val nextOctopuses =
            current ++ adjacent(head).flatMap { p =>
              current.get(p).map(v => (p, v + 1))
            }.toMap
          val lit =
            nextOctopuses.filter(_._2 > 9).toList.map(_._1).filterNot(visited)
          loop(visited ++ lit.toSet, tail ++ lit, nextOctopuses)
      }

      loop(initialLit.toSet, initialLit, bumped).transform { case (_, v) =>
        if (v > 9) 0 else v
      }
    }

    def numLit = value.values.count(_ == 0)

    def allFlash = value.values.forall(_ == 0)
  }

  val steps = LazyList.iterate(init)(_.next)

  override def partOne(): String =
    steps.drop(1).take(100).map(_.numLit).sum.toString

  override def partTwo(): String =
    steps.zipWithIndex.dropWhile(!_._1.allFlash).head._2.toString
}
