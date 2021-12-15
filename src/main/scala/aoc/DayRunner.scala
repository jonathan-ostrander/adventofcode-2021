package aoc

import java.time.Instant
import java.util.Calendar
import scala.reflect.runtime.universe
import scala.util.Try

object DayRunner extends App {
  val day = Try(args(0)).toOption.getOrElse(Calendar.getInstance().get(Calendar.DAY_OF_MONTH).toString)
  val rm = universe.runtimeMirror(getClass.getClassLoader)
  val objectName = if (day.toInt < 10) s"Day0$day" else s"Day$day"
  
  val dayObject: Day =
    rm.reflectModule(rm.staticModule(s"aoc.$objectName").asModule).instance.asInstanceOf[aoc.Day]

  def time(f: => Unit): Unit = {
    val start = Instant.now
    f
    println(s"Took: ${Instant.now.toEpochMilli() - start.toEpochMilli()} ms")
  }

  time(println(s"Part 1: ${dayObject.partOne()}"))
  time(println(s"Part 2: ${dayObject.partTwo()}"))
}
