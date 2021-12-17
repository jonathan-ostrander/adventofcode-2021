package aoc

object Day17 extends InputReader(17) with Day {
  val t = raw"target area: x=(\-?\d+)..(\-?\d+), y=(\-?\d+)..(\-?\d+)".r
  val target: ((Int, Int), (Int, Int)) =
    input.head match {
      case t(x1, x2, y1, y2) =>
        ((x1.toInt, x2.toInt), (y1.toInt, y2.toInt))
      case _ => sys.error("bad input")
    }
  val minY = target._2._1
  val maxY = target._2._1.abs - 1

  val minX = {
    val x = target._1._1
    (0 to x).find(i => ((i * (i + 1)) / 2) > x).get
  }
  val maxX = target._1._2

  override def partOne(): String = (((maxY + 1) * maxY) / 2).toString

  implicit class RichPosition(value: Int) {
    def between(boundary: (Int, Int)): Boolean =
      boundary._1 <= value && value <= boundary._2
  }

  def hitsTarget(x: Int, y: Int): Boolean = {
    def loop(pos: (Int, Int), vel: (Int, Int)): Boolean = {
      if (pos._1 > target._1._2 || pos._2 < target._2._1) false
      else if (pos._1.between(target._1) && pos._2.between(target._2)) true
      else {
        val newXVelocity = if (vel._1 == 0) 0 else if (vel._1 > 0) vel._1 - 1 else vel._1 + 1
        loop(
          (pos._1 + vel._1, pos._2 + vel._2),
          (newXVelocity, vel._2 - 1)
        )
      }
    }
    loop(0 -> 0, x -> y)
  }

  override def partTwo(): String =
    (
      for {
        x <- minX to maxX
        y <- minY to maxY
        if hitsTarget(x, y)
      } yield (x, y)
    ).length.toString
}
