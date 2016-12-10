package me.max.wittmann.hackerrank

import scala.io.StdIn

object HAEquals {

  val DEBUG = false

  def main(args:Array[String]): Unit = {

    for (testCaseAt <- 0 until StdIn.readLine().toInt) {
      StdIn.readLine()
      val input = StdIn.readLine().split(" ").map(_.toInt).toList
      val result = solve(input)
      println(result)
    }

//    solve(List(2, 2, 3, 7))
  }


  def solve(candies: List[Int]): Int = {
    val min = candies.min
    if (DEBUG)
      println(s"Min: $min")

    val amountToDeduct = (min / 5) * 5
    val extraAmountToDeduct = min - amountToDeduct

    val normalizedCandies = candies
      .map(_ - amountToDeduct)
      .filter(_ > 0)

    (0 to extraAmountToDeduct)
      .map(atd => {
        val s1 = normalizedCandies.map(_ - atd)
        val s2 = s1.map(v => (v / 5) + ((v % 5) / 2) + (v % 5 % 2))
        val s3 = s2.sum

        if (DEBUG)
          println(s"For ($amountToDeduct + $atd) ${s1.mkString(", ")}\n${s2.mkString(", ")}\n$s3")

        s3
      })
      .min
  }
}
