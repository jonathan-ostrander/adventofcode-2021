package aoc

import scala.io.Source
import scala.sys.process._

abstract class Day(day: Int) {
  private[this] val fileName = if (day > 9) s"day$day.txt" else s"day0$day.txt"

  def input: List[String] =
    try {
      Source.fromResource(fileName).getLines.toList
    } catch {
      case _: java.io.FileNotFoundException =>
        val session = sys.env.getOrElse("AOC_SESSION", sys.error("No session cookie set."))
        s"curl https://adventofcode.com/2021/day/$day/input -H 'cookie: session=$session' -o src/main/resources/$fileName".!
        input
    }

  def partOne(): String
  def partTwo(): String
}
