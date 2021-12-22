package aoc

object Day22 extends Day(22) {
  implicit class RichRange(r: Range) {
    def overlap(other: Range): Option[Range] =
      if (other.start < r.start) other.overlap(r)
      else if (other.start <= r.end) Some(other.start to other.end.min(r.end))
      else None
  }

  case class Cuboid(x: Range, y: Range, z: Range) {
    override def toString(): String =
      s"[(${x.start},${x.end}),(${y.start},${y.end}),(${z.start},${z.end})], size: $size"
    def size: Long = x.size.toLong * y.size.toLong * z.size.toLong

    def nonEmpty: Boolean = x.nonEmpty && y.nonEmpty && z.nonEmpty
    def intersect(other: Cuboid): Option[Cuboid] =
      for {
        xi <- x.overlap(other.x)
        yi <- y.overlap(other.y)
        zi <- z.overlap(other.z)
      } yield Cuboid(xi, yi, zi)

    
    def +(other: Cuboid): List[Cuboid] = intersect(other) match {
      case None => this :: other :: Nil
      case Some(value) =>
        value :: (this - value) ++ (other - value)
    }
    def -(other: Cuboid): List[Cuboid] = intersect(other) match {
      case None => this :: Nil
      case Some(removed) =>
        // prisms from x.start until removed.x.start in the X direction
        // and from removed.x.end to x.end
        // includes full y and z
        val xs = this.copy(x = x.start to (removed.x.start - 1)) :: this.copy(x = (removed.x.end + 1) to x.end) :: Nil
        // prisms not including xs starting going in the y direction includes full z
        val ys = Cuboid(removed.x, y.start to (removed.y.start - 1), z) :: Cuboid(removed.x, (removed.y.end + 1) to y.end, z) :: Nil
        // leftover z
        val zs = removed.copy(z = z.start to (removed.z.start - 1)) :: removed.copy(z = (removed.z.end + 1) to z.end) :: Nil
        (xs ++ ys ++ zs).filter(_.nonEmpty)
    }
  }

  sealed trait Instruction {
    def cuboid: Cuboid
    def apply(state: List[Cuboid]): List[Cuboid]
  }
  case class On(cuboid: Cuboid) extends Instruction{
    def apply(state: List[Cuboid]): List[Cuboid] = {
      def loop(cuboids: List[Cuboid], toMerge: List[Cuboid]): List[Cuboid] =
        toMerge match {
          case Nil => cuboids
          case head :: tail =>
            val (noMerge, merge) = cuboids.partition(_.intersect(head).isEmpty)
            merge match {
              case Nil => loop(head :: cuboids, tail)
              case m :: rest =>
                val postMerge = head + m
                loop(rest ++ noMerge, tail ++ postMerge)
            }
        }
      loop(state, cuboid :: Nil)
    }
  }
  case class Off(cuboid: Cuboid) extends Instruction{
    override def apply(state: List[Cuboid]): List[Cuboid] = state.flatMap(_ - cuboid)
  }


  val instructions = input.map { line =>
    val (on, xyz) = line.split(" ").toList match {
      case "on" :: xyz :: Nil => true -> xyz
      case "off" :: xyz :: Nil => false -> xyz
      case _ => sys.error(s"bad line: $line")
    }
    xyz.split(",").map(_.drop(2).split("\\.\\.").map(_.toInt).toList).toList match {
      case (x1 :: x2 :: Nil) :: (y1 :: y2 :: Nil) :: (z1 :: z2 :: Nil) :: Nil =>
        val cuboid = Cuboid(x1 to x2, y1 to y2, z1 to z2)
        if (on) On(cuboid) else Off(cuboid)
      case _ => sys.error(s"bad line: $line")
    }
  }

  val start = instructions.take(1).map(_.cuboid)

  lazy val finalCuboids = instructions.tail.zipWithIndex.foldLeft(start) { case (acc, (instruction, i)) =>
    println(s"Step $i / ${instructions.length - 1}. Cuboid list currently at ${acc.length}")
    instruction(acc)
  }
  lazy val finalSum = finalCuboids.map(_.size).sum

  override def partOne(): String =
    (finalSum - Off(Cuboid(-50 to 50, -50 to 50, -50 to 50))(finalCuboids).map(_.size).sum).toString
  override def partTwo(): String = finalSum.toString
}
