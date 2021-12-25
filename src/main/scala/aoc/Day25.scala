package aoc

object Day25 extends Day(25) {
  val (xBound, yBound) = (input.length, input.head.length)
  def inputSet(f: Char): Set[(Int, Int)] =
    for {
      (line, x) <- input.zipWithIndex.toSet
      (char, y) <- line.zipWithIndex
      if char == f
    } yield (x, y)

  val initialEast = inputSet('>')
  val initialSouth = inputSet('v')

  implicit class Cucumbers(positions: (Set[(Int, Int)], Set[(Int, Int)])) {
    def moved = positions != move

    lazy val move: (Set[(Int, Int)], Set[(Int, Int)]) = {
      val eastMove = positions._1.map {
        case (x, y) =>
          val nextSpot = x -> ((y + 1) % yBound)
          if (positions._1(nextSpot) || positions._2(nextSpot)) x -> y
          else nextSpot
      }
      eastMove -> positions._2.map {
        case (x, y) =>
          val nextSpot = ((x + 1) % xBound) -> y
          if (eastMove(nextSpot) || positions._2(nextSpot)) x -> y
          else nextSpot
      }
    }
  }

  override def partOne(): String = (
    LazyList.iterate(initialEast -> initialSouth)(_.move)
      .zipWithIndex
      .dropWhile(_._1.moved)
      .head._2 + 1
  ).toString
  
  override def partTwo(): String = "???"  
}
