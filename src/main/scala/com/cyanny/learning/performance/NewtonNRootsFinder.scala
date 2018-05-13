package com.cyanny.learning.performance

// https://www.cse.wustl.edu/~kjg/cse131/Notes/SquareRoot/sqrt.html
object NewtonNRootsFinder {

  def findRout(w: Double, n: Int): Double = {
    def f(w: Double, g: Double) = math.pow(g, n) - w
    def fPrime(g: Double) = n * math.pow(g, n - 1)
    def isCloseEnough(a: Double, b: Double) = math.abs(a - b) < Math.abs(b * 0.001)
    def findRoutInternal(w: Double, g: Double): Double = {
      println(s"w: $w, guessing: $g")
      val newGuess = g - f(w, g) / fPrime(g)
      if (isCloseEnough(newGuess, g)) {
        newGuess
      } else {
        findRoutInternal(w, newGuess)
      }
    }
    findRoutInternal(w, 1)
  }

  def main(args: Array[String]): Unit = {
    val v = findRout(27.0, 3)
    println(v)
  }

}
