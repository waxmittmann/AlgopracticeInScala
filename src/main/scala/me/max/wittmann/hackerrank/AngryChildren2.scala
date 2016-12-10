package me.max.wittmann.hackerrank

object AngryChildren2 {

  def main (args: Array[String]): Unit = {
    println(solve(List(10, 100, 300, 200, 1000, 20, 30), 3))
  }

  def solve(candies: List[Int], children: Int): Int = {
    val prefixes =
      candies
        .sorted
        .foldLeft(candies.take(1))((li, cur) => {
          (cur + li.head) :: li
        })
        .reverse

//    (0 until candies.length - 1 - children).map(i => {
//      prefixes(i + children - 1) - prefixes(i)
//    }).min






    (children + 1 until candies.length - 1).map(i => {

    })

  }

}
