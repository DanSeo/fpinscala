package datastructure

import org.scalatest.FunSuite

class SpeedTest extends FunSuite {
  test("Should return 15 of list ") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List.sum(list) == 15)
  }

  test("Should return removed list") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List[Int](1, 2, 3, 4, 5) == List[Int](1, 2, 3, 4, 5))
  }
  
  test("Should return revered list") {
    val list = List[Int](1,2,3,4,5)
    assert(List.reverse[Int](list) == List[Int](5,4,3,2,1))
  }

}
