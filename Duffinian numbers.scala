object DuffinianExample extends App {
  def factors(n: Int): List[Int] = {
    (1 to n).filter(n % _ == 0).toList
  }

  def gcd(a: Int, b: Int): Int = {
    var x = a
    var y = b
    while (y != 0) {
      val temp = x
      x = y
      y = temp % y
    }
    x
  }

  def isRelativelyPrime(a: Int, b: Int): Boolean = gcd(a, b) == 1

  def sigmaSum(x: Int): Int = factors(x).sum

  def isDuffinian(x: Int): Boolean = {
    isRelativelyPrime(x, sigmaSum(x)) && factors(x).size > 2
  }

  var count = 0
  var i = 0
  while (count < 50) {
    if (isDuffinian(i)) {
      print(i + " ")
      count += 1
    }
    i += 1
  }
  println()

  var count2 = 0
  var j = 0
  while (count2 < 20) {
    if (isDuffinian(j) && isDuffinian(j+1) && isDuffinian(j+2)) {
      print(s"($j,${j+1},${j+2}) ")
      count2 += 1
      j += 3
    }
    j += 1
  }
  println()
}