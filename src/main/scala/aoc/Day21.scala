package aoc

import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.mutable.ParTrieMap

object Day21 extends Day(21) {
  val startingPlayers = input.map(_.drop(28).toInt -> 0).map(t => Player(t._1, t._2)) match {
    case first :: second :: Nil => first -> second
    case _ => sys.error("bad input")
  }

  case class Player(position: Int, score: Int) {
    def go(roll: Int): Player = {
      val newPos = {
        val p = (position + roll) % 10
        if (p == 0) 10 else p
      }
      Player(newPos, (score + newPos))
    }
  }

  override def partOne(): String = {
    def loop(players: (Player, Player), rolls: Int): Int = {
      val move = (1 to 3).map(i => (i + rolls) % 100).sum
      val newRolls = rolls + 3
      val nextPlayer = players._1.go(move)

      if (nextPlayer.score >= 1000) players._2.score * newRolls
      else loop(players._2 -> nextPlayer, newRolls)
    }

    loop(startingPlayers, 0).toString
  }

  override def partTwo(): String = {
    val possibleDiceRolls = for {
      i <- (1 to 3); j <- (1 to 3); k <- (1 to 3)
    } yield i + j + k
    val rollDistribution = possibleDiceRolls.groupBy(identity).transform { case (_, l) => l.length }.toList.par

    def winDistribution(state: ParTrieMap[(Player, Player), AtomicLong], soFar: (Long, Long)): Long =
      if (state.isEmpty) soFar._1 max soFar._2
      else {
        val wins = new AtomicLong(soFar._1)
        val stateMap = TrieMap.empty[(Player, Player), AtomicLong]

        rollDistribution.foreach {
          case (roll, numRolls) => state.foreach {
            case ((player, otherPlayer), quant) =>
              val newPlayer = player.go(roll)
              val longRef =
                if (newPlayer.score >= 21) wins
                else stateMap.getOrElseUpdate((otherPlayer, newPlayer), new AtomicLong(0L))
              longRef.addAndGet(numRolls * quant.get())
          }
        }

        winDistribution(stateMap.par, (soFar._2, wins.get))
      }
    winDistribution(TrieMap((startingPlayers._1, startingPlayers._2) -> new AtomicLong(1L)).par, (0, 0)).toString
  }
}
