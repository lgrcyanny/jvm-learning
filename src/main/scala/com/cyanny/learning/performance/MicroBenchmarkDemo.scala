package com.cyanny.learning.performance

import scala.util.Random


class MicroBenchmarkDemo {
  @volatile private var l: Double = 0
  private var nLoops: Int = 0
  private var input: Array[Int] = null

  def fibonacciTest(nLoops: Int, isWarmUp: Boolean = false) = {
    this.nLoops = nLoops
    this.input = new Array[Int](nLoops)
    val random = new Random()
    for (i <- 0 until nLoops) {
      input(i) = random.nextInt(50)
    }
    val startTime = System.currentTimeMillis()
    for (i <- 0 until nLoops) {
      this.l = fipImpl(this.input(i))
      println(s"fibonacci value: ${l}")
    }
    if (!isWarmUp) {
      println(s"Elapsed time: ${System.currentTimeMillis() - startTime}ms")
    }

  }

  def fipImpl(n: Int): Double = {
    if (n < 0) throw new IllegalArgumentException(s"n < 0: $n")
    if (n == 0) {
      0d
    } else if (n == 1) {
      1d
    } else {
      val d = fipImpl(n - 1) + fipImpl(n - 2)
      if (java.lang.Double.isInfinite(d)) throw new ArithmeticException("Overflow")
      d
    }
  }

}

object MicroBenchmarkDemo {
  def main(args: Array[String]): Unit = {
    val tester = new MicroBenchmarkDemo
//    tester.fibonacciTest(10)
    tester.fipImpl(100)
  }
}
