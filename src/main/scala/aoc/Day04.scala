package aoc

object Day04 extends InputReader(4) with Day {
  case class Board(rows: List[Set[Int]], columns: List[Set[Int]]) {
    def all = rows ++ columns

    def mark(v: Int): Board = Board(
      rows.map(_ - v),
      columns.map(_ - v),
    )
    def isWinner: Boolean = all.exists(_.isEmpty)
    def score(lastNum: Int): Int = rows.flatten.sum * lastNum
  }

  object Board {
    def parse(rows: List[String]): Board = {
      val rowNums = rows.map(_.split(" ").filterNot(_ == "").map(_.toInt).toList)
      Board(
        rows = rowNums.map(_.toSet),
        columns = rowNums.transpose.map(_.toSet),
      )
    }
  }

  val numbers = input.head.split(",").map(_.toInt).toList 
  val boards = input.drop(2).mkString("\n").split("\n\n").map {
    case board => Board.parse(board.split("\n").toList)
  }.toList

  def loop(left: List[Int], boards: List[Board], f: List[Board] => Option[Board]): String =
    left match {
      case Nil => sys.error("Exit condition never met")
      case next :: tail =>
        val nextBoards = boards.map(_.mark(next))
        f(nextBoards)
          .map(_.score(next).toString)
          .getOrElse(loop(tail, nextBoards.filterNot(_.isWinner), f))
    }

  override def partOne(): String = {
    loop(numbers, boards, _.find(_.isWinner))
  }

  override def partTwo(): String = {
    loop(numbers, boards, _ match {
      case Nil => sys.error("No winning boards")
      case head :: Nil if head.isWinner => Some(head)
      case _ => None
    })
  }
}
