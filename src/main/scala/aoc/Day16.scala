package aoc

object Day16 extends Day(16) {
  implicit class BinaryString(value: String) {
    def fromBinary: BigInt = BigInt(value, 2)
    def hexToBinary: String = {
      val s = BigInt(value, 16).toString(2)
      List.fill(s.length % 4)('0').mkString + s
    }
  }

  case class Packet(
      version: Int,
      typeId: Int,
      value: PacketValue
  ) {
    def versionSum: Int = value match {
      case Literal(value)       => version
      case Operator(subpackets) => version + subpackets.map(_.versionSum).sum
    }

    def evaluate: BigInt = value match {
      case Literal(value) => value
      case Operator(subpackets) =>
        val evaluated = subpackets.map(_.evaluate)
        val operation = Packet.operationMap(typeId)
        evaluated.reduce(operation)
    }
  }

  object Packet {
    val operationMap: Map[Int, (BigInt, BigInt) => BigInt] = Map(
      0 -> (_ + _),
      1 -> (_ * _),
      2 -> (_ min _),
      3 -> (_ max _),
      5 -> ((a, b) => if (a > b) 1 else 0),
      6 -> ((a, b) => if (a < b) 1 else 0),
      7 -> ((a, b) => if (a == b) 1 else 0)
    )

    def parse(binary: String): (Packet, String) = {
      val version = binary.take(3).fromBinary.toInt
      val id = binary.drop(3).take(3).fromBinary.toInt
      val rest = binary.drop(6)

      if (id == 4) {
        val (literal, next) = parseLiteral(rest, "")

        Packet(version, id, literal) -> next
      } else if (rest.head == '0') {
        val subpacketLength = rest.drop(1).take(15).fromBinary.toInt

        Packet(
          version,
          id,
          parseOperatorByLength(rest.drop(16).take(subpacketLength), Nil)
        ) -> rest.drop(16 + subpacketLength)
      } else {
        val (operator, next) = parseOperatorByNumber(
          rest.drop(12),
          rest.drop(1).take(11).fromBinary.toInt
        )
        Packet(version, id, operator) -> next
      }
    }

    def parseLiteral(left: String, bitsSoFar: String): (Literal, String) = {
      val (head, bits, tail) =
        (left.head, bitsSoFar + left.tail.take(4), left.tail.drop(4))
      head match {
        case '1' => parseLiteral(tail, bits)
        case '0' => Literal(bits.fromBinary) -> tail
        case _   => sys.error(s"not a binary string: $left")
      }
    }

    def parseOperatorByLength(
        subpackets: String,
        packets: List[Packet]
    ): Operator =
      subpackets match {
        case "" => Operator(packets)
        case _ =>
          val (packet, left) = parse(subpackets)
          parseOperatorByLength(left, packets :+ packet)
      }

    def parseOperatorByNumber(left: String, num: Int): (Operator, String) =
      (0 until num).foldLeft(Operator(Nil) -> left) {
        case ((Operator(acc), s), _) =>
          val (packet, n) = parse(s)
          Operator(acc :+ packet) -> n
      }
  }

  sealed trait PacketValue
  case class Literal(value: BigInt) extends PacketValue
  case class Operator(subpackets: List[Packet]) extends PacketValue

  val packet = Packet.parse(input.head.hexToBinary)._1

  override def partOne(): String = packet.versionSum.toString

  override def partTwo(): String = packet.evaluate.toString

}
