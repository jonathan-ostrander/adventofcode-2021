package aoc

object Day06 extends InputReader(6) with Day {
  val initialFish =
    input.head.split(",")
      .map(_.toInt)
      .groupBy(identity)
      .transform { case (_, arr) => arr.length.toLong }

  implicit class School(fish: Map[Int, Long]) {
    def nextDay: Map[Int, Long] = {
      val newFish = fish.getOrElse(0, 0L)
      val nonReproducing = fish
        .filter(_._1 != 0)
        .map { case (t, v) => (t-1, v) }
      nonReproducing ++
        Map(
          (6, nonReproducing.getOrElse(6, 0L) + newFish),
          (8, nonReproducing.getOrElse(8, 0L) + newFish),
        )
    }
  }

  def solve(days: Int): String =
    LazyList.iterate(initialFish)(_.nextDay)
      .drop(days).head.map(_._2).sum.toString

  override def partOne(): String = solve(80)

  override def partTwo(): String = solve(256)
}
