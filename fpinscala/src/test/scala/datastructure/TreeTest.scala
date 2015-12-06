package datastructure

import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("should return size of tree ") {
    assert(7 == Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(11 == Tree.size(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return the number of maximum  of tree ") {
    assert(5 == Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(10 == Tree.maximum(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return the maximum depth of tree ") {
    assert(2 == Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(3 == Tree.depth(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }
}