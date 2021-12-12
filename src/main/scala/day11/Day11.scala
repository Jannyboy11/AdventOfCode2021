package day11;

import scala.annotation.tailrec
import scala.io.Source

val source = Source.fromResource("day11.in")
val input: Octopuses = source.getLines().map(string => string.map((c: Char) => c - '0').toIndexedSeq).toIndexedSeq

type Octopuses = IndexedSeq[IndexedSeq[Int]]
type Point = (Int, Int)
type Flashes = Set[Point]

extension (octopuses: Octopuses)
    def apply(y: Int, x: Int): Int = octopuses(y)(x)
    def apply(point: Point): Int = apply(point._1, point._2)
    def updated(y: Int, x: Int, level: Int): Octopuses = octopuses.updated(y, octopuses(y).updated(x, level))
    def updated(point: Point, level: Int): Octopuses = updated(point._1, point._2, level)
    def height: Int = octopuses.length
    def width: Int = octopuses(0).length
    def prettyPrint(): Unit = octopuses.foreach(row => println(row.mkString))

def adjacent(y: Int, x: Int, height: Int, width: Int): Seq[Point] = {
    val points: Seq[Point] = Seq(
        (y-1, x-1), (y-1, x), (y-1, x+1),
        (y, x-1), /*(y, x), */ (y, x+1),
        (y+1, x-1), (y+1, x), (y+1, x+1)
    )
    points.filter { case (y, x) => 0 <= y && y < height && 0 <= x && x < width }
}

def tryFlash(y: Int, x: Int, octopuses: Octopuses, alreadyFlashed: Flashes): (Octopuses, Flashes) = {
    val point = (y, x)
    if (alreadyFlashed.contains(point)) {
        (octopuses, alreadyFlashed)
    } else if (octopuses(point) > 9) {
        var ocs = octopuses.updated(point, 0)
        var flashes = alreadyFlashed + point
        for ((adjY, adjX) <- adjacent(y, x, octopuses.height, octopuses.width)) {
            ocs = ocs.updated(adjY, adjX, ocs(adjY, adjX) + 1)
            val (o, f) = tryFlash(adjY, adjX, ocs, flashes);
            ocs = o; flashes = f;
        }
        (ocs, flashes)
    } else {
        (octopuses, alreadyFlashed)
    }
}

def step(octopuses: Octopuses): (Octopuses, Int) = {
    var newOctopuses = octopuses.map(_.map(_ + 1))
    var flashes: Flashes = Set()
    for (y <- 0 until octopuses.height) {
        for (x <- 0 until octopuses.width) {
            val (newOcs, newFlashes) = tryFlash(y, x, newOctopuses, flashes)
            newOctopuses = newOcs
            flashes = newFlashes
        }
    }
    for (p <- flashes) {
        newOctopuses = newOctopuses.updated(p, 0)
    }
    //newOctopuses.prettyPrint()
    (newOctopuses, flashes.size)
}

@tailrec def go(limit: Int, octopuses: Octopuses, total: Int): Int = limit match {
    case 0 => total
    case x =>
        val (newOcs, flashes) = step(octopuses)
        val newTotal = total + flashes
        go(limit - 1, newOcs, newTotal)
}

@main def main: Unit = {

    val result1 = go(100, input, 0)
    println(result1)

    val result2 = {
        val octopusCount: Int = input.height * input.width
        var flashes: Int = 0
        var octopuses: Octopuses = input
        var steps: Int = 0
        while (flashes != octopusCount) {
            val (o, f) = step(octopuses)
            octopuses = o; flashes = f; steps += 1
        }
        steps
    }
    println(result2)

}