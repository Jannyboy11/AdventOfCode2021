package day07

import scala.io.Source

val source = Source.fromResource("day07.in")
val input: List[Int] = source.getLines().next().split(",").map(_.toInt).toList

def minMax(list: List[Int]): (Int, Int) =
    list.foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE)) { case ((min, max), value) => (Math.min(min, value), Math.max(max, value)) }
def cost1(targetPos: Int, positions: List[Int]): Int =
    positions.map((pos: Int) => Math.abs(targetPos - pos)).sum
def cost2(targetPos: Int, positions: List[Int]): Int =
    positions.map { (pos: Int) =>
        val distance = Math.abs(targetPos - pos)
        (distance * (distance + 1)) / 2 //use gauss formula
    }.sum

@main def main: Unit = {

    val (min, max) = minMax(input)
    val result1 = (min to max).map(pos => cost1(pos, input)).min
    println(result1)
    val result2 = (min to max).map(pos => cost2(pos, input)).min
    println(result2)

}