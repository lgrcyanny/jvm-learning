package com.cyanny.learning.performance

import java.math.RoundingMode
import java.math.BigDecimal

object BigDecimalDemo {
  val TWO = new BigDecimal(2)

  /**
    * Babylonian method for estimating the square root of a number
    * @param bd
    * @param error
    * @return
    */
  def sqrt(bd: BigDecimal, error: BigDecimal = new BigDecimal(1e-6)): BigDecimal = {
    var initial = bd
    var diff: BigDecimal = null
    do {
      val sDivX = bd.divide(initial, 8, RoundingMode.FLOOR)
      val sum = sDivX.add(initial)
      val div = sum.divide(TWO, 8, RoundingMode.FLOOR)
      diff = div.subtract(initial).abs()
      diff.setScale(8, RoundingMode.FLOOR)
      initial = div
    } while (diff.compareTo(error) > 0)
    initial
  }

  def sqrtNewton(bd: BigDecimal): BigDecimal = {
    def newGuess(x: BigDecimal, g: BigDecimal) = {
      (g.add(x.divide(g, 8, RoundingMode.FLOOR)).divide(TWO, 8, RoundingMode.FLOOR))
    }
    def isCloseEnough(a: BigDecimal, b: BigDecimal) = {
      (a.subtract(b).abs()).compareTo(b.multiply(new BigDecimal(0.001))) < 0
    }
    def sqrtInternal(x: BigDecimal, g: BigDecimal): BigDecimal = {
      if (isCloseEnough(x.divide(g, 8, RoundingMode.FLOOR), g)) {
        g
      } else {
        val betterGuess = newGuess(x, g)
        sqrtInternal(x, betterGuess)
      }
    }
    sqrtInternal(bd, new BigDecimal(1.0))
  }

  def withTime[T](fn: => T): T = {
    val startTime = System.currentTimeMillis()
    val res = fn
    val endTime = System.currentTimeMillis()
    println(s"time taken: ${endTime - startTime}ms")
    res
  }

  def main(args: Array[String]): Unit = {
    val a = withTime(sqrt(new BigDecimal(2.0)))
    println(a.doubleValue())

    val b = withTime(sqrtNewton(new BigDecimal(2.0)))
    println(b.doubleValue())
  }

}
