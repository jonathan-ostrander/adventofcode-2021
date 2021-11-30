package aoc

abstract class InputReader(day: Int) extends App {
  def input: List[String] =
    scala.io.Source.fromResource(if (day > 9) s"day$day.txt" else s"day0$day.txt").getLines.toList
}
