package aoc

object Day10 extends InputReader(10) with Day {
  val pairs = Map(
    ')' -> ('(', 3L, 1L),
    ']' -> ('[', 57L, 2L),
    '}' -> ('{', 1197L, 3L),
    '>' -> ('<', 25137L, 4L),
  )
  val reversePairs = pairs.map(p => p._2._1 -> p._1)

  def score(line: String): Either[Long, Long] = {
    def corruptScore(char: Char): Long = pairs(char)._2
    def incompleteScore(chars: List[Char]): Long =
      chars.map(reversePairs).foldLeft(0L)((acc, c) => (acc * 5) + pairs(c)._3)

    def loop(left: List[Char], stack: List[Char]): Either[Long, Long] =
      (left, stack) match {
        case (Nil, Nil) => Right(0)
        case (head :: tail, Nil) => loop(tail, head :: Nil)
        case (Nil, rest) => Right(incompleteScore(rest))
        case (head :: tail, top :: nextStack) =>
          pairs.get(head) match {
            case None => loop(tail, head :: stack)
            case Some(c) =>
              if (c._1 == top) loop(tail, nextStack)
              else Left(corruptScore(head))
          }
      }
    loop(line.toList, Nil)
  }

  val scores = input.map(score)

  override def partOne(): String =
    scores.collect { case Left(v) => v}.sum.toString
  override def partTwo(): String = {
    val incompleteScores = scores.flatMap(_.toOption).sorted
    incompleteScores.drop(incompleteScores.length / 2).head.toString
  }
}
