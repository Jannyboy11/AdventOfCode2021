package day08

import scala.annotation.tailrec
import scala.io.Source

val source = Source.fromResource("day08.in")
val input: Seq[(Seq[Digit], Seq[Digit])] = source.getLines().map(line => {
    val Array(input, output) = line.split(" \\| ")
    val inputs: Seq[Digit] = input.split(" ").toSeq.map(_.toSet.asInstanceOf[Digit])
    val outputs: Seq[Digit] = output.split(" ").toSeq.map(_.toSet.asInstanceOf[Digit])
    (inputs, outputs)
}).toSeq

type Segment = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g'
type Digit = Set[Segment]

def computeNumber(digit: Seq[Int]): Int =
    digit.foldRight((1, 0)) { case (digit, (base, total)) => (base*10, total + base*digit) }._2
extension [A] (set: Set[A])
    def containsAll(other: Set[A]): Boolean = other.subsetOf(set)

@main def main: Unit = {

    val result1 = input.flatMap(_._2).count(digit => { val len = digit.size; len == 2 || len == 4 || len == 3 || len == 7 })
    println(result1)

    val result2 = input.map { case (patterns, outputs) =>
        val one = patterns.find(_.size == 2).get
        val four = patterns.find(_.size == 4).get
        val seven = patterns.find(_.size == 3).get
        val eight = patterns.find(_.size == 7).get
        val three = patterns.find(set => set.size == 5 && set.containsAll(one)).get
        val nine = patterns.find(set => set.size == 6 && set.containsAll(four)).get
        val e: Segment = eight.diff(nine).head
        val two = patterns.find(set => set.size == 5 && set.contains(e)).get
        val five = patterns.find(set => set.size == 5 && !set.contains(e) && set != three).get
        val six = patterns.find(set => set.size == 6 && set.containsAll(five) && set.contains(e)).get
        val zero = patterns.find(set => set.size == 6 && set.contains(e) && set != six).get

        val number = outputs.map(d => Seq(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex.find { case (digit, i) => digit == d } .get._2)
        computeNumber(number)
    }.sum
    println(result2)

}
