package laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {
  test("drop stream") {
    val data = Stream(1, 2, 3, 5)
    assert(Stream(3, 5).toList == data.drop(2).toList)
  }

  test("take stream") {
    val stream = Stream(1, 2, 3, 4)
    assert(Stream(1, 2).toList == stream.take(2).toList)
  }

  test("take while numbers divided by 2 no remain") {
    val stream = Stream(2, 2, 3, 4)
    assert(Stream(2, 2).toList == stream.takeWhile { _ % 2 == 0 }.toList)
  }

  test("take while Via foldRight numbers divided by 2 no remain") {
    val stream = Stream(2, 2, 3, 4)
    assert(Stream(2, 2).toList == stream.takeWhileViaFoldRight { _ % 2 == 0 }.toList)
  }

  test("head option Via foldRight ") {
    val stream = Stream(1, 2, 3, 4)
    assert(Some(1) == stream.headOptionViaFoldRight)
  }

  test("head option2 Via foldRight ") {
    val stream = Stream(1, 2, 3, 4)
    assert(Some(1) == stream.headOption2ViaFoldRight)
  }

  test("fromViaUnfold take 5") {
    assert(Stream(5, 6, 7, 8, 9, 10).toList == Stream.fromViaUnfold(5).take(5).toList)
  }

  test("onesViaUnfold take 5") {
    assert(Stream(1, 1, 1, 1, 1).toList == Stream.onesViaViaUnfold.take(5).toList)
  }

  test("constantsViaUnfold take 5") {
    assert(Stream(2, 2, 2, 2, 2).toList == Stream.constantViaUnfold(2).take(5).toList)
  }

  test("fibsViaUnfold take 10") {
    assert(Stream.fibs.take(10).toList == Stream.fibsViaUnfold.take(10).toList)
  }

}