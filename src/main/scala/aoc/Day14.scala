package aoc

object Day14 extends InputReader(14) with Day {
  val template = input.head.sliding(2).toList.groupBy(identity).transform { case (_, v) => v.length.toLong }
  val rules = input.drop(2).map(_.split(" -> ").toList).collect {
    case k :: v :: Nil => k -> (k.take(1) + v, v + k.drop(1))
  }.toMap

  def apply(value: Map[String, Long]): Map[String, Long] =
    value.toList
      .flatMap { case (pair, n) =>
        rules.get(pair)
          .map { case (a, b) => List(a -> n, b -> n) }
          .getOrElse(List(pair -> n))
      }
      .groupBy(_._1)
      .transform { case (_, v) => v.map(_._2).sum }

  def loop(n: Int): String = {
    val result = Stream.iterate(template)(apply).drop(n).head.toList
      .map { case (k, v) => k.head -> v }
      .groupBy(_._1)
      .transform { case (_, vs) => vs.map(_._2) }
      .map { case (k, vs) => if (k == input.head.last) vs.sum + 1 else vs.sum }
    (result.max - result.min).toString
  }
  override def partOne(): String = loop(10)
  override def partTwo(): String = loop(40)
}
