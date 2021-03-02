import cb03.Week1
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LeetCodeTest extends AnyFunSuite with Matchers{

  test("should return half maximum variable number count") {
    Week1.distributeCandies(Array(1,1,2,2,3,3)) shouldBe 3
    Week1.distributeCandies(Array(1,1,2,2,2,2)) shouldBe 2
    Week1.distributeCandies(Array(1,2,3,4)) shouldBe 2
  }

  test("should return duplicate and missed number") {
    Week1.findErrorNums(Array(1,2,2)) shouldBe Array(2, 3)
    Week1.findErrorNums(Array(1,3,3,4)) shouldBe Array(3, 2)
    Week1.findErrorNums3(Array(1,3,3,4)) shouldBe Array(3, 2)
  }
}
