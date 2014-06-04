package com.nthalk.util.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.nthalk.util.BST

class BSTSpec extends FlatSpec with Matchers {

  "A BST" should "be able to add an element" in {
    val bst = new BST[Int]((a, b) => { a - b });
    bst.add(1)
    bst.getRootNode.get.el should be(1)
  }

  "A BST" should "be able to construct a left leg" in {
    val bst = new BST[Int]((a, b) => { a - b });
    bst.add(1)
    bst.add(2)
    bst.add(3)
    val root = bst.getRootNode.get
    root.el should be(1)
    root.right.get.el should be(2)
    root.right.get.right.get.el should be(3)
  }

  "A BST" should "be able to construct a right leg" in {
    val bst = new BST[Int]((a, b) => { a - b });
    bst.add(3)
    bst.add(2)
    bst.add(1)
    val root = bst.getRootNode.get
    root.el should be(3)
    root.left.get.el should be(2)
    root.left.get.left.get.el should be(1)
  }

  "A BST" should "be able to construct a teepee" in {
    val bst = new BST[Int]((a, b) => { a - b });
    bst.add(2)
    bst.add(3)
    bst.add(1)
    val root = bst.getRootNode.get
    root.el should be(2)
    root.left.get.el should be(1)
    root.right.get.el should be(3)
  }

  "A BST" should "be able to iterate" in {
    val range = (1 to 8)
    range.permutations.foreach { list =>
      val bst = new BST[Int]((a, b) => { a - b });
      list.foreach { bst.add _ }
      val itr = bst.iterator
      for (i <- range) {
        itr.hasNext should be(true)
        itr.next should be(i)
      }
      itr.hasNext should be(false)
    }

  }

}