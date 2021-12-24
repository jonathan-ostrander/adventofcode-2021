package aoc

import scala.collection.mutable

object Day23 extends Day(23) {
  abstract class Amphipod(val room: Int) {
    val moveCost = Math.pow(10, room).toInt
  }
  case object A extends Amphipod(0)
  case object B extends Amphipod(1)
  case object C extends Amphipod(2)
  case object D extends Amphipod(3)

  val initialHallway = IndexedSeq(None, None, None, None, None, None, None)

  val initialRooms =
    input.drop(2).dropRight(1).map(_.split("#").filter(_.length == 1).filterNot(_.isEmpty)).transpose
      .map(_.collect { case "A" => A; case "B" => B; case "C" => C; case "D" => D })
      .zipWithIndex.map { case (l, i) => Room(i, l, l.length) }
      .toIndexedSeq

  val fold = IndexedSeq(List(D, D), List(C, B), List(B, A), List(A, C))
  val initialBiggerRooms = initialRooms.zip(fold).map {
    case (r, f) =>
      val newPods = r.pods.head :: f ++ r.pods.tail
      r.copy(pods = newPods, capacity = newPods.length)
  }

  implicit class Hallway(value: IndexedSeq[Option[Amphipod]]) {
    def isClear: Boolean = value.forall(_.isEmpty)

    def roomToHallwayClear(hallway: Int, room: Int): Boolean =
      if (hallway > room + 2) value.slice(room + 2, hallway).isClear
      else value.slice(hallway + 1, room + 2).isClear
    def betweenRoomsClear(roomA: Int, roomB: Int): Boolean =
      if (roomA > roomB) betweenRoomsClear(roomB, roomA)
      else value.slice(roomA + 2, roomB + 2).isClear
  }

  case class Room(index: Int, pods: List[Amphipod], capacity: Int) {
    def pop: Room = this.copy(pods = pods.tail)
    def add(pod: Amphipod): Room = if (isFor(pod)) this.copy(pods = pod :: pods) else this

    def topPodCanMove: Boolean = pods.exists(!isFor(_))

    def isAvailable: Boolean = pods.forall(isFor)
    def isFull: Boolean = isAvailable && pods.length == capacity

    def isFor(pod: Amphipod): Boolean = pod.room == index

    def movesToGetIn: Int = capacity - pods.length
    def movesToGetOut: Int = movesToGetIn + 1

    def distanceToRoom(other: Room): Int =
      movesToGetOut + 2 * (index - other.index).abs + other.movesToGetIn

    def distanceFromHallwayToRoomOpening(hallway: Int): Int = {
      val (moveOutOfCorner, nonCorner) =
        if (hallway == 0) 1 -> 1
        else if (hallway == 6) 1 -> 5
        else 0 -> hallway

      val mult = if (nonCorner < index + 2) 1 else -1
      moveOutOfCorner + 2 * mult * (index - nonCorner) + (mult * 3)
    }
    def distanceFromHallwaySpot(hallway: Int): Int =
      distanceFromHallwayToRoomOpening(hallway) + movesToGetIn
    def distanceToHallwaySpot(hallway: Int): Int =
      distanceFromHallwayToRoomOpening(hallway) + movesToGetOut
  }

  case class State(
    cost: Long,
    hallway: IndexedSeq[Option[Amphipod]],
    rooms: IndexedSeq[Room],
  ) {
    lazy val innerState = (hallway, rooms)
    override def hashCode(): Int = innerState.hashCode()
    override def equals(other: Any): Boolean = other match {
      case s: State => innerState.equals(s.innerState)
      case _ => false
    }

    lazy val zippedHallway = hallway.zipWithIndex

    def done = hallway.isClear && rooms.forall(_.isFull)

    lazy val availableRooms = rooms.filter(_.isAvailable)

    lazy val roomsWithPodsToMove = rooms.filter(_.topPodCanMove)
    lazy val roomToRoom = roomsWithPodsToMove.filter { room =>
      availableRooms.exists(_.isFor(room.pods.head)) && hallway.betweenRoomsClear(room.index, room.pods.head.room)
    }
    lazy val hallwayToRoom = for {
      (maybePod, hallwayIndex) <- zippedHallway
      pod <- maybePod
      if availableRooms.exists(_.isFor(pod))
      if hallway.roomToHallwayClear(hallwayIndex, pod.room)
    } yield pod -> hallwayIndex

    def moves: List[State] = {
      val hallwayToRoomMoves = hallwayToRoom.map { case (pod, hallwayIndex) =>
          State(
            cost + (pod.moveCost * rooms(pod.room).distanceFromHallwaySpot(hallwayIndex)),
            zippedHallway.map { case (p, i) => if (i == hallwayIndex) None else p },
            rooms.map(_.add(pod)),
          )
      }

      val roomToRoomMoves = roomToRoom.map { room =>
        val pod = room.pods.head
        val toRoom = rooms(pod.room)
        State(
          cost + (pod.moveCost * room.distanceToRoom(toRoom)),
          hallway,
          rooms.map(r => if (room == r) r.pop else r.add(pod)),
        )
      }

      val roomToHallwayMoves =
        roomsWithPodsToMove.flatMap { room =>
          val pod = room.pods.head
          zippedHallway.collect {
            case (None, hallwayIndex) if hallway.roomToHallwayClear(hallwayIndex, room.index) =>
              State(
                cost + (pod.moveCost * room.distanceToHallwaySpot(hallwayIndex)),
                zippedHallway.map { case (p, i) => if (i == hallwayIndex) Some(pod) else p },
                rooms.map(r => if (room == r) room.pop else r),
              )
        }
      }
      
      roomToRoomMoves.toList ++ hallwayToRoomMoves.toList ++ roomToHallwayMoves.toList
    }
  }

  implicit val ordering: Ordering[State] = Ordering.by[State, Long](_.cost).reverse

  def solve(rooms: IndexedSeq[Room]): String = {
    val stateQueue = mutable.PriorityQueue[State](State(0, initialHallway, rooms))
    val seen = mutable.Set.empty[State]

    def loop(): Long = {
      val nextState = stateQueue.dequeue()
      if (nextState.done) {
        nextState.cost
      } else if (seen(nextState)) loop()
      else {
        val _ = seen.add(nextState)
        val moves = nextState.moves.filterNot(seen)
        val _ = stateQueue.addAll(moves)
        loop()
      }
    }
    loop().toString
  }

  override def partOne(): String = solve(initialRooms)
  override def partTwo(): String = solve(initialBiggerRooms)
}
