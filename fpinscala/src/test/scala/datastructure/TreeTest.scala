package datastructure

import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("should return size of tree") {
    val numberTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))
    val extendedTree = Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))
    assert(7 == Tree.size(numberTree))
    assert(11 == Tree.size(extendedTree))
  }

  test("should return the number of maximum of tree") {
    assert(5 == Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(10 == Tree.maximum(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return the maximum depth of tree") {
    assert(2 == Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(3 == Tree.depth(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return results using map method") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val resultTree = Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6)))
    assert(resultTree == Tree.map(tree)(v => v + 2))
  }

  test("should return size of tree Using sizeViaFold") {
    val numberTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))
    val extendedTree = Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))
    assert(7 == Tree.sizeViaFold(numberTree))
    assert(11 == Tree.sizeViaFold(extendedTree))
  }

  test("should return the number of maximum  of tree using maximumViaFold") {
    assert(5 == Tree.maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(10 == Tree.maximumViaFold(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return the maximum depth of tree using depthViaFold") {
    assert(2 == Tree.depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5)))))
    assert(3 == Tree.depthViaFold(Branch(Branch(Leaf(6), Leaf(10)), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(5))))))
  }

  test("should return results using mapViaFold method") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val resultTree = Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6)))
    assert(resultTree == Tree.mapViaFold(tree)(v => v + 2))
  }
}