package datastructure

import org.scalatest.FunSuite

class SpeedTest extends FunSuite {
  test("Should return 15 of list ") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List.sum(list) == 15)
  }

  // TODO List Should add == method.
  test("Should return removed list") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List[Int](1, 2, 3, 4, 5) == List[Int](1, 2, 3, 4, 5))
  }

}
