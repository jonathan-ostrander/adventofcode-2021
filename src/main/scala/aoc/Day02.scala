package aoc

object Day02 extends Day(2) {
  case class Submarine(horizontal: Int, depth: Int, aim: Int) {
    def applyCommand(command: Command, correct: Boolean) =
      command.direction match {
        case Command.Forward =>
          if (correct) Submarine(horizontal + command.value, depth + (aim * command.value), aim)
          else Submarine(horizontal + command.value, depth, aim)
        case Command.Depth =>
          if (correct) Submarine(horizontal, depth, aim + command.value)
          else Submarine(horizontal, depth + command.value, aim)
      }
  }

  case class Command(direction: Command.Direction, value: Int)

  object Command {
    sealed trait Direction
    case object Forward extends Direction
    case object Depth extends Direction

    def parse(value: String): Command = {
      val (d :: v :: Nil) = value.split(" ").take(2).toList
      d match {
        case "forward" => Command(Forward, v.toInt)
        case "up" => Command(Depth, v.toInt * -1)
        case "down" => Command(Depth, v.toInt)
      }
    }
  }

  def solve(correct: Boolean) = {
    val finalSub = input.map(line => Command.parse(line)).foldLeft(Submarine(0, 0, 0)) {
      case (sub, command) => sub.applyCommand(command, correct)
    }
    (finalSub.depth * finalSub.horizontal).toString()
  }
  override def partOne(): String = solve(false)
  override def partTwo(): String = solve(true)
}
