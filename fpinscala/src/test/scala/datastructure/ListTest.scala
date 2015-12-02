package datastructure

import org.scalatest.FunSuite

class SpeedTest extends FunSuite {
  test("should return 15 of list ") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List.sum(list) == 15)
  }

  test("should return removed list") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List[Int](1, 2, 3, 4, 5) == List[Int](1, 2, 3, 4, 5))
  }

  test("should return revered list") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List.reverse[Int](list) == List[Int](5, 4, 3, 2, 1))
  }

  test("should append list") {
    val list = List[Int](1, 2, 3, 4, 5)
    assert(List.appendViaFoldLeft(list, 6) == List[Int](1, 2, 3, 4, 5, 6))

    println(List.plusOne(List[Int](1, 2, 3, 4, 5)))
  }

  test("should return the results of plusOne method") {
    val ints = List[Int](1, 2, 3, 4, 5)
    assert(ints != List.plusOne(ints))
  }

  test("should return the results of converting string") {
    val numbers = List[Double](1.0, 2.0, 3.3, 3.4)

    assert(List[String]("1.0", "2.0", "3.3", "3.4") == List.convertDoubleToString(numbers))
  }
  
  test("should return the results of map method") {
    val numbers = List[Int](1,2,3,4,5)
    
    assert(List[Int](10,20,30,40,50) == List.map(numbers)((a:Int) => a * 10))
    println(numbers)
  }
}
