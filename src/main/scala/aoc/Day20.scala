package aoc

object Day20 extends Day(20) {
  val algo = input.head.zipWithIndex.filter(_._1 == '#').map(_._2).toSet

  case class Image(litInBounds: Set[(Int, Int)], bounds: ((Int, Int), (Int, Int)), outOfBoundsLit: Boolean) {
    val ((minX, minY), (maxX, maxY)) = bounds

    def isLit(point: (Int, Int)): Boolean = {
      val (x, y) = point

      if (minX <= x && x <= maxX && minY <= y && y <= maxY) litInBounds(point)
      else outOfBoundsLit
    }

    def enhance: Image = {
      val seq = for {
        x <- (minX - 1) to (maxX + 1)
        y <- (minY - 1) to (maxY + 1)
        b = Integer.parseInt(square(x, y).map(isLit).map(b => if (b) '1' else '0').mkString, 2)
        if algo(b)
      } yield (x, y)
      Image(
        seq.toSet,
        ((minX - 1, minY - 1), (maxX + 1, maxY + 1)),
        (!outOfBoundsLit && algo(0)) || (outOfBoundsLit && algo(255)),
      )
    }
  }
    
  def square(x: Int, y: Int): Seq[(Int, Int)] =
    for {
      i <- -1 to 1
      j <- -1 to 1
    } yield (x + i, y + j)

  val initialImage = {
    val set = input.drop(2).zipWithIndex.flatMap {
      case (line, x) => line.zipWithIndex.collect {
        case ('#', y) => x -> y
      }
    }.toSet
    Image(
      set,
      ((set.map(_._1).min, set.map(_._2).min), (set.map(_._1).max, set.map(_._2).max)),
      false,
    )
  }

  def loop(n: Int): String =
    LazyList.iterate(initialImage)(_.enhance).drop(n).head.litInBounds.size.toString
  override def partOne(): String = loop(2)
  
  override def partTwo(): String = loop(50)
  
}