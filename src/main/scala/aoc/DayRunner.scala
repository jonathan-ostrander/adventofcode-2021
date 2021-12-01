package aoc

import scala.reflect.runtime.universe

object DayRunner extends App {
  val day = args(0)
  val rm = universe.runtimeMirror(getClass.getClassLoader)
  val objectName = if (day.toInt < 10) s"Day0$day" else s"aoc.Day$day"
  
  val dayObject: Day =
    rm.reflectModule(rm.staticModule(s"aoc.$objectName").asModule).instance.asInstanceOf[aoc.Day]

  println(s"Part 1: ${dayObject.partOne()}")
  println(s"Part 2: ${dayObject.partTwo()}")
}
