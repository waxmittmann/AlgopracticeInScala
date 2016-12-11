package me.max.wittmann.hackerrank

import scala.io.StdIn

object AngryChildren2 {

  val DEBUG = true
  def dPrint(str: String): Unit = {
    if (DEBUG)
      println(str)
    else
      Unit
  }

  def main (args: Array[String]): Unit = {

    val candiesNr = StdIn.readLine().toInt
    val kidsNr = StdIn.readLine().toInt

    val candies = (0 until candiesNr).toList.map(i => {
      val v = StdIn.readLine().toInt
      dPrint(s"Read $i $v")
      v
    })

    dPrint(s"Candies: $candies, Kids: $kidsNr")

    val result = solve(candies, kidsNr)
    dPrint("Done readin'")
    println(result)

    //println(solve(List(10, 100, 300, 200, 1000, 20, 30), 3))
  }

  def solve(candies: List[Int], children: Int): Int = {
    val candiesS = candies.sorted

    val prefixes =
      candiesS.drop(1)
        .foldLeft(candiesS.take(1))((li, cur) => {
          val newLi = (cur + li.head) :: li
          dPrint(s"Cur: $cur, li.head: ${li.head}, New: ${cur + li.head}, Li: $li, NewLi: $newLi")
          newLi
        })
        .reverse

    val results = Array.fill(candiesS.length)(Integer.MAX_VALUE)
    // Initialise by calculating the first value in O(children)
    //(0 until children - 1).foreach(i => results(i))
    results(children - 1) = 0
    for (i <- 1 until children - 1) {
      //We need to use diffs, not candiesS
      dPrint(s"At $i, adding ${(i+1) * candiesS(i)}")
      //results(children - 1) += (i+1) * candiesS(i)

      results(children - 1) += (i+1) * (candiesS(i) - candiesS(i-1))
    }

    for (i <- children until candies.length) {
      results(i) = results(i-1) +
                    children * candiesS(i) +
                    prefixes(i-1) - prefixes(i-children)
    }

    val bestResult = results.min

    dPrint(
      s"""
         |Prefixes: $prefixes
         |Results:  ${results.toList}
         |Best:     $bestResult
       """.stripMargin)

    bestResult
  }

}
