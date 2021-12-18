package aoc

object Day13 extends Day(13) {
  val points =
    input.takeWhile(!_.isEmpty).map(_.split(",").map(_.toInt).toList).collect {
      case x :: y :: Nil => x -> y
    }

  sealed trait Fold {
    def value: Int

    def fold(point: (Int, Int)): Option[(Int, Int)] =
      if (point._1 == value) None
      else if (point._1 < value) Some(point)
      else Some((2 * value - point._1) -> point._2)

    def apply(points: List[(Int, Int)]): List[(Int, Int)] = points.flatMap(fold)
  }
  case class X(value: Int) extends Fold
  case class Y(value: Int) extends Fold {
    override def apply(points: List[(Int, Int)]): List[(Int, Int)] =
      super.apply(points.map(_.swap)).map(_.swap)
  }

  val folds =
    input
      .dropWhile(!_.isEmpty)
      .drop(1)
      .map(_.drop(11).split("=").toList)
      .collect {
        case "x" :: num :: Nil => X(num.toInt)
        case "y" :: num :: Nil => Y(num.toInt)
      }

  implicit class RichPoints(points: List[(Int, Int)]) {
    def print: String = {
      val maxX = points.map(_._1).max
      val maxY = points.map(_._2).max

      val pointSet = points.toSet
      (0 to maxY)
        .map { y =>
          (0 to maxX).map(x => if (pointSet(x -> y)) '#' else ' ').mkString
        }
        .mkString("\n")
    }
  }

  override def partOne(): String =
    folds.head.apply(points).toSet.size.toString
  override def partTwo(): String =
    "\n" + folds.foldLeft(points) { (acc, fold) => fold(acc) }.print
}
