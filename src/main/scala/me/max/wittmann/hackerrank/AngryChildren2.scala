package me.max.wittmann.hackerrank

object AngryChildren2 {

  def main (args: Array[String]): Unit = {
    println(solve(List(10, 100, 300, 200, 1000, 20, 30), 3))
  }

  def solve(candies: List[Int], children: Int): Int = {
    val candiesS = candies.sorted

    val prefixes =
      candiesS
        .foldLeft(candiesS.take(1))((li, cur) => {
          (cur + li.head) :: li
        })
        .reverse

//    (0 until candies.length - 1 - children).map(i => {
//      prefixes(i + children - 1) - prefixes(i)
//    }).min

    val seedValue =
      (0 to children-1).toList
        .map(i => (0 to children - 1).toList
          .map(j => {
            Math.abs(candiesS(i) - candiesS(j))
          }).sum).sum

    println(s"Prefixes: $prefixes\nSeed value: $seedValue")

    val diffs =
      (children to candiesS.length - 1).foldLeft(List(seedValue))((r, i) => {
        val i_minus_1: Int = r.head

        val i_diff: Int = candiesS(i) * children - (prefixes(i) - prefixes(i - children)) //i-children-1?

        val i_minus_k_diff: Int = candiesS(i - children) * children - (prefixes(i) - prefixes(i - children)) //i-children-1?

        (i_minus_1 + i_diff - i_minus_k_diff) :: r
      })

    println(s"Diffs: $diffs")

    val minDiff = diffs.min / 2

    minDiff


//    (children + 1 until candies.length - 1).map(i => {
//
//    })
  }

}
