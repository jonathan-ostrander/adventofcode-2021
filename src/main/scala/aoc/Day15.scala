package aoc

import scala.collection.mutable

object Day15 extends Day(15) {
  val smallGrid = input.zipWithIndex.flatMap { case (line, x) =>
    line.zipWithIndex.map { case (c, y) => (x, y) -> c.toString.toInt }
  }.toMap

  val smallGridX = smallGrid.map(_._1._1).max + 1
  val smallGridY = smallGrid.map(_._1._2).max + 1

  def mapped(
      point: (Int, Int),
      value: Int,
      mult: (Int, Int)
  ): ((Int, Int), Int) = {
    val p =
      (point._1 + (mult._1 * smallGridX), point._2 + (mult._2 * smallGridY))
    val n = value + mult._1 + mult._2
    p -> (if (n > 9) n - 9 else n)
  }

  val bigGrid = {
    val horizontal = (0 to 4).flatMap(i =>
      smallGrid.map { case (p, v) => mapped(p, v, (i, 0)) }
    )
    (0 to 4)
      .flatMap(j => horizontal.map { case (p, v) => mapped(p, v, (0, j)) })
      .toMap
  }

  def dijkstra(grid: Map[(Int, Int), Int]): Int = {
    val queue = mutable.PriorityQueue((0, 0, 0))
    val visited = mutable.Set.empty[(Int, Int)]
    val target = (grid.map(_._1._1).max, grid.map(_._1._2).max)

    def loop(): Int = {
      val next = queue.dequeue()
      if (next._2 == target._1 && next._3 == target._2) -1 * next._1
      else if (visited(next._2 -> next._3)) loop()
      else {
        val adjacent = List(-1 -> 0, 1 -> 0, 0 -> -1, 0 -> 1)
          .map { case (i, j) =>
            (next._2 + i, next._3 + j)
          }
          .flatMap {
            case p if !visited(p) =>
              grid.get(p).map(v => (next._1 - v, p._1, p._2))
            case _ => None
          }
        queue.addAll(adjacent)
        visited.add(next._2 -> next._3)
        loop()
      }
    }
    loop()
  }

  override def partOne(): String = dijkstra(smallGrid).toString

  override def partTwo(): String = dijkstra(bigGrid).toString
}
