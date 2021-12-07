package aoc

object Day07 extends InputReader(7) with Day {
  val crabs = input.head.split(",").map(_.toInt).toList

  sealed trait FuelConsumption {
    def singleFuelCost(from: Int, to: Int): Int
    def totalFuelCost(pos: Int): Int = crabs.map(crab => singleFuelCost(crab, pos)).sum
    def minFuelCost: String = (0 to crabs.max).map(totalFuelCost).min.toString
  }
  case object Constant extends FuelConsumption {
    override def singleFuelCost(from: Int, to: Int): Int = (from - to).abs
  }
  case object Linear extends FuelConsumption {
    override def singleFuelCost(from: Int, to: Int): Int = {
      val n = Constant.singleFuelCost(from, to)
      (n * (n + 1)) / 2
    }
  }

  override def partOne(): String = Constant.minFuelCost
  override def partTwo(): String = Linear.minFuelCost
}
