package aoc

object Day12 extends InputReader(12) with Day {
  val graph: Map[String, List[String]] =
    input.map(_.split("-").toList.take(2))
      .foldLeft(Map.empty[String, List[String]]) {
        case (acc, left :: right :: Nil) =>
          acc ++ Map(
            left -> (right :: acc.getOrElse(left, Nil)),
            right -> (left :: acc.getOrElse(right, Nil)),
          )
        case _ => sys.error("Bad input")
      }

  case class Path(
    visited: Set[String],
    current: String,
    canRevisit: Option[String],
    didRevisit: Option[String],
  ) {
    def isComplete: Boolean = current == "end"
    def nextPaths: List[Path] = {
      val (newVisited, newCanRevisit, newDidRevisit) =
        if (current.head.isUpper) (visited, canRevisit, didRevisit)
        else if (canRevisit.exists(_ == current)) (visited, None, Some(current))
        else if (didRevisit.exists(_ == current)) (visited + current, None, None)
        else (visited + current, canRevisit, didRevisit)
      graph.getOrElse(current, Nil)
        .filterNot(visited)
        .map(next => Path(newVisited, next, newCanRevisit, newDidRevisit))
    }
  }

  def solve(paths: List[Path], completed: Int): Int = paths match {
    case Nil => completed
    case path :: tail =>
      if (path.isComplete && path.canRevisit.isEmpty && path.didRevisit.isEmpty) {
        solve(tail, completed + 1)
      }
      else if (path.isComplete) solve(tail, completed)
      else solve(path.nextPaths ++ tail, completed)
  }

  override def partOne(): String =
    solve(Path(Set.empty, "start", None, None) :: Nil, 0).toString
  override def partTwo(): String = {
    val allTheSmallThings =
      graph.keys.filterNot(k => k.head.isUpper || k == "start" || k == "end").map(Some.apply).toList
    solve((None :: allTheSmallThings).map(canRevisit => Path(Set.empty, "start", canRevisit, None)), 0).toString
  }
}
