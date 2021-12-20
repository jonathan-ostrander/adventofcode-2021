package aoc

object Day19 extends Day(19) {
  implicit class Beacon(val point: (Int, Int, Int)) {
    val (x, y, z) = point

    override def equals(obj: Any): Boolean = obj match {
      case b: Beacon => point == b.point
      case _ => false
    }

    override def hashCode(): Int = point.hashCode()

    def dist(other: Beacon): Beacon =
      (other.x - x, other.y - y, other.z - z)
    def +(other: Beacon): Beacon =
      (other.x + x, other.y + y, other.z + z)

    def rotations: List[Beacon] =
      List((x, y, z), (z, y, -x), (-x, y, -z), (-z, y, x), (x, -z, y), (x, z, -y))
        .flatMap { case (i, j, k) => List((i, j, k), (j, -i, k), (-i, -j, k), (-j, i, k)) }

    /**
     * Move this point to a different scanner's relative system
     *
     * @param toMoveTo Root beacon to move to relative to root scanner
     * @param equivalent Equivalent beacon in this beacon's scanner's system
     * @return Relative position of this beacon to the root scanner
     */
    def translate(toMoveTo: Beacon, equivalent: Beacon): Beacon =
      this + equivalent.dist(toMoveTo)
  }

  case class Scanner(beacons: List[Beacon]) {
    def rotations: List[Scanner] =
      beacons.map(_.rotations).transpose.toList.map(Scanner.apply)

    def overlaps(other: Scanner): Option[(Set[Beacon], Beacon)] = {
      val beaconSet = beacons.toSet
      val overlap = for {
        root <- beacons
        rotation <- other.rotations
        rotationRoot <- rotation.beacons
        rotationRootTranslated = rotationRoot.translate(root, rotationRoot)
        translated = rotation.beacons.map(_.translate(root, rotationRoot)).toSet
        if beaconSet.intersect(translated).size >= 12
      } yield (beaconSet ++ translated, rotationRoot.dist(root))
      overlap.headOption
    }
  }

  val scanners: List[Scanner] =
    input.filter(_.nonEmpty).foldLeft(List.empty[Scanner]) {
      case (acc, line) =>
        if (line.startsWith("---")) Scanner(Nil) :: acc
        else {
          val x :: y :: z :: Nil = line.split(",").map(_.toInt).toList
          Scanner(acc.head.beacons :+ Beacon(x, y, z)) :: acc.tail
        }
    }.reverse

  def loop(
    cur: Set[Beacon],
    points: List[Beacon],
    toMerge: List[Scanner],
  ): (Set[Beacon], List[Beacon]) = {
    toMerge match {
      case Nil => cur -> points
      case head :: tail => 
        println(toMerge.size)
        Scanner(cur.toList).overlaps(head) match {
          case Some((newCur, dist)) => loop(newCur, dist :: points, tail)
          case None => loop(cur, points, tail :+ head)
        }
    }
  }
  val (merged, points) = loop(scanners.head.beacons.toSet, List((0, 0, 0)), scanners.tail)
  override def partOne(): String = merged.size.toString
  override def partTwo(): String =
    points.combinations(2).collect {
      case a :: b :: Nil =>
        val d = a.dist(b)
        d.x.abs + d.y.abs + d.z.abs
    }.max.toString
}
