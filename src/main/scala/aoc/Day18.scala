package aoc

import java.util.concurrent.atomic.AtomicReference

object Day18 extends Day(18) {
  sealed trait Tree {
    def magnitude: Long

    def +(other: Tree) = new Branch(this, other).reduce

    def explode: Option[Tree] = {
      def loop(stack: List[Branch], visited: Set[Branch]): Option[Tree] = stack match {
        case Nil => None
        case head :: tail =>
          (head.left, head.right) match {
            case (Leaf(l), Leaf(r)) if tail.length >= 4 =>
              val parent = tail.head
              if (head == parent.left) {
                parent.left = Leaf(0)

                parent.right match {
                  case Leaf(value) =>
                    parent.right = Leaf(value + r)
                  case b: Branch =>
                    b.addToFirstLeft(r)
                }

                stack.sliding(2).find(s => s(0) == s(1).right) match {
                  case Some(_ :: p :: Nil) =>
                    p.left match {
                      case Leaf(value) =>
                        p.left = Leaf(value + l)
                      case b: Branch =>
                        b.addToFirstRight(l)
                    }
                  case _ => ()
                }
              } else {
                parent.right = Leaf(0)

                parent.left match {
                  case Leaf(value) =>
                    parent.left = Leaf(value + l)
                  case b: Branch =>
                    b.addToFirstRight(l)
                }

                stack.sliding(2).find(s => s(0) == s(1).left) match {
                  case Some(_ :: p :: Nil) =>
                    p.right match {
                      case Leaf(value) =>
                        p.right = Leaf(value + r)
                      case b: Branch =>
                        b.addToFirstLeft(r)
                    }
                  case _ => ()
                }
              }
              stack.lastOption
            case (b: Branch, _) if !visited(b) => loop(b :: stack, visited)
            case (_, b: Branch) if !visited(b) => loop(b :: stack, visited)
            case _ => loop(tail, visited + head)
          }
      }
      this match {
        case Leaf(value) => None
        case b: Branch => loop(b :: Nil, Set.empty)
      }
    }

    def split: Option[Tree] = {
      def loop(stack: List[Branch], visited: Set[Branch]): Option[Tree] = stack match {
        case Nil => None
        case head :: tail =>
          (head.left, head.right) match {
            case (l@Leaf(v), _) if v > 9 =>
              head.left = l.splitLeaf
              stack.lastOption
            case (b: Branch, _) if !visited(b) => loop(b :: stack, visited)
            case (_, l@Leaf(v)) if v > 9 =>
              head.right = l.splitLeaf
              stack.lastOption
            case (_, b: Branch) if !visited(b) => loop(b :: stack, visited)
            case _ => loop(tail, visited + head)
          }
      }
      this match {
        case Leaf(value) => None
        case b: Branch => loop(b :: Nil, Set.empty)
      }
    }

    def reduceOnce: Option[Tree] = explode.orElse(split)

    def reduce: Tree = {
      def loop(current: Tree): Tree =
        current.reduceOnce match {
          case Some(value) => loop(value)
          case None => current
        }

      loop(this)  
    }
  }
  case class Leaf(value: Int) extends Tree {
    override def toString(): String = value.toString
    override def magnitude: Long = value.toLong

    def splitLeaf: Branch = new Branch(
      Leaf(value / 2),
      if (value % 2 == 0) Leaf(value / 2) else Leaf(value / 2 + 1),
    )
  }
  class Branch(initialLeft: Tree, initialRight: Tree) extends Tree {
    var left = initialLeft
    var right = initialRight

    override def toString(): String = s"[${left},${right}]"
    override def magnitude: Long = 3*left.magnitude + 2*right.magnitude

    def addToFirstLeft(value: Int): Unit = left match {
      case Leaf(v) => left = Leaf(v + value)
      case b: Branch => b.addToFirstLeft(value)
    }

    def addToFirstRight(value: Int): Unit = right match {
      case Leaf(v) => right = Leaf(v + value)
      case b: Branch => b.addToFirstRight(value)
    }
  }

  object Tree {
    def parse(string: String): Tree = {
      def loop(stringLeft: String): (Tree, String) = 
        if (stringLeft.head.isDigit) {
          Leaf(stringLeft.takeWhile(_.isDigit).toInt) -> stringLeft.dropWhile(_.isDigit)
        } else {
          val (left, rightString) = loop(stringLeft.drop(1))
          val (right, next) = loop(rightString.drop(1))
          new Branch(left, right) -> next.drop(1)
        }

      loop(string)._1
    }
  }

  override def partOne(): String =
    input.map(Tree.parse).reduce(_ + _).magnitude.toString
  override def partTwo(): String =
    input.combinations(2).map {
      case a :: b :: _ =>
        val ab = Tree.parse(a) + Tree.parse(b)
        val ba = Tree.parse(b) + Tree.parse(a)
        ab.magnitude max ba.magnitude
      case _ => 0
    }.max.toString
}
