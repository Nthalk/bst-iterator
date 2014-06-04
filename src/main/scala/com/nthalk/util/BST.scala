package com.nthalk.util

class BST[T](compare: (T, T) => Int) extends Iterable[T] {

  sealed abstract class OrderOutcome(verifier: Int => Boolean) {
    def unapply(i: Int) = verifier(i)
  }
  case object AGTB extends OrderOutcome(_ > 0)
  case object ALTB extends OrderOutcome(_ < 0)
  case object AEQB extends OrderOutcome(_ == 0)

  case class Node[T](el: T, var parent: Option[Node[T]], var left: Option[Node[T]] = None, var right: Option[Node[T]] = None)

  var root: Option[Node[T]] = None

  def getRootNode = root

  def add(el: T): Unit = {
    root match {
      case Some(node) =>
        add(el, node)
      case _ =>
        root = Some(Node(el, None))
    }
  }

  def iterator = new Iterator[T] {
    var current = decendLeft

    def decendLeft: Option[Node[T]] = {
      root match {
        case Some(node) =>
          decendLeft(node)
        case _ =>
          None
      }
    }

    def decendLeft(node: Node[T]): Option[Node[T]] = {
      node.left match {
        case Some(left) =>
          decendLeft(left)
        case _ =>
          Some(node)
      }
    }

    def hasNext: Boolean = {
      current.isDefined
    }
    def next: T = {
      val ret = current.get.el

      // We have already decended left
      val node = current.get
      if (node.right.isDefined) {
        // We have a right
        // Current(B), Next(C)
        //    B
        //  A   D
        //     C
        
        // Move to D then down to C
        current = decendLeft(node.right.get)
      } else if (node.parent.isDefined) {
        // We have a parent
        if (current == node.parent.get.right) {
          // We are the right leg, crawl up it
          //   Current(A), Next(D)
          //      D
          //     C
          //      B
          //       A

          // Crawl up to C
          while (current.get.parent.isDefined && current == current.get.parent.get.right) {
            current = current.get.parent
          }

          // Then crawl to parent D
          current = current.get.parent
        } else {
          // Crawl up the left leg
          //  Current(A), Next(B)
          //   B
          //  A C
          current = node.parent
        }
      } else {
        // We could not descend right or move up
        // We must be done
        current = None
      }
      ret
    }

  }

  def add(el: T, node: Node[T]): Unit = {
    compare(el, node.el) match {
      case AGTB() =>
        if (node.right.isDefined) {
          add(el, node.right.get)
        } else {
          node.right = Some(Node(el, Some(node)))
        }
      case AEQB() =>
      // Do nothing, duplicate node
      case ALTB() =>
        if (node.left.isDefined) {
          add(el, node.left.get)
        } else {
          node.left = Some(Node(el, Some(node)))
        }
    }
  }
}
