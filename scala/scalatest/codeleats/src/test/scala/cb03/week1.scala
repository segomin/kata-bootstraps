package cb03

import scala.collection.mutable

object Week1 {
  def distributeCandies(candyType: Array[Int]): Int = {
    Math.min(candyType.toSet.size, candyType.size / 2);
  }

  def findErrorNums(nums: Array[Int]): Array[Int] = {
    val hashSet = mutable.Set[Int]()
    var dup = 0;
    var sum = 0
    for(num <- nums) {
      if (hashSet.contains(num)) {
        dup = num;
      }
      hashSet.add(num)
      sum += num;
    }
    val ant = (nums.length + 1) * nums.length / 2
    Array(dup, ant - sum + dup)
  }

  // quite fast
  def findErrorNums2(nums: Array[Int]): Array[Int] = {
    var xor, xor1, xor2 = 0
    for(n <- nums) xor ^= n
    for(n <- 1 to nums.length) xor ^= n
    val rightmostbit = xor & ~(xor - 1)
    for(n <- nums) if((rightmostbit & n) == 0) xor1 ^= n else xor2 ^= n
    for(n <-  1 to nums.length) if((rightmostbit & n) == 0) xor1 ^= n else xor2 ^= n
    for(n <- nums) if(n == xor1) return Array(xor1, xor2) else if(n == xor2) return Array(xor2, xor1)
    Array(xor1, xor2)
  }

  // most fast
  def findErrorNums3(nums: Array[Int]): Array[Int] = {
    var dup = -1
    var missing = -1

    for (i <- 0 until nums.length) {
      val j = math.abs(nums(i)) - 1
      if (nums(j) < 0) dup = j else nums(j) = -nums(j)
    }

    for (i <- 0 until nums.length) {
      if (nums(i) > 0) missing = i
    }

    Array(dup + 1, missing + 1)
  }
}