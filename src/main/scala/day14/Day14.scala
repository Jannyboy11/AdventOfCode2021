package day14

import scala.io.Source

val source = Source.fromResource("day14.in")
val lines = source.getLines()

val template: Seq[Char] = { val t = lines.next().toSeq; lines.next(); t }
val polymer: Polymer = Polymer(template)

val rules: Rules = lines.map(row => {
    val Array(pattern, between) = row.split(" -> ")
    ((pattern.charAt(0), pattern.charAt(1)), between.charAt(0))
}).toMap

type Bond = (Char, Char)
type Connections = Map[Bond, Long]
type Rules = Map[Bond, Char]
type Counts = Map[Char, Long]

enum InsertAttempt:
    case Success(a: Char, between: Char, b: Char, matches: Long)
    case Failure

object InsertAttempt:
    def apply(polymer: Polymer, matcher: Bond, between: Char): InsertAttempt = polymer.connections.get(matcher) match
        case Some(matches) => InsertAttempt.Success(matcher._1, between, matcher._2, matches)
        case None => InsertAttempt.Failure

object Polymer:
    def apply(template: Seq[Char]): Polymer = {
        val connections = template.zip(template.tail).groupMapReduce(identity)(_ => 1L)(_ + _)
        val counts = template.groupMapReduce(identity)(_ => 1L)(_ + _)
        Polymer(connections, counts)
    }

case class Polymer(connections: Connections, counts: Counts):
    def +(attempt: InsertAttempt): Polymer = attempt match
        case InsertAttempt.Success(a, between, b, matches) =>
            val conn = connections.updatedWith((a, b)) { case Some(count) => Some(count - matches).filter(_ > 0L) }
                .updatedWith((a, between)) { case Some(count) => Some(count + matches); case None => Some(matches) }
                .updatedWith((between, b)) { case Some(count) => Some(count + matches); case None => Some(matches) }
            val c = counts.updatedWith(between) { case Some(count) => Some(count + matches); case None => Some(matches) }
            Polymer(conn, c)
        case InsertAttempt.Failure => this

def applyRules(polymer: Polymer, rules: Rules): Polymer =
    rules.map(InsertAttempt(polymer, _, _)).foldLeft(polymer)(_ + _)

def go(steps: Int, polymer: Polymer, rules: Rules): Polymer = {
    var i = 0; var acc = polymer
    while i < steps do { acc = applyRules(acc, rules); i += 1 }
    acc
}

def computeResult(steps: Int, polymer: Polymer, rules: Rules): Long = {
    val poly = go(steps, polymer, rules)
    poly.counts.values.max - poly.counts.values.min
}

@main def main: Unit = {

    val result1 = computeResult(10, polymer, rules)
    println(result1)

    val result2 = computeResult(40, polymer, rules)
    println(result2)

}