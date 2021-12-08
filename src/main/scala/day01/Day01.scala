package day01

import scala.io.Source;

val source = Source.fromResource("day01.in")
val input = source.getLines.map(_.toInt).toList

@main def main: Unit = {

    val t1 = input.tail
    val t2 = t1.tail

    val result1 = input.zip(t1).count((depth1, depth2) => depth2 > depth1)
    println(result1)

    val sums = input.zip(t1).zip(t2).map { case ((one, two), three) => one + two + three }
    val result2 = sums.zip(sums.tail).count((s1, s2) => s2 > s1);
    println(result2)
    
}