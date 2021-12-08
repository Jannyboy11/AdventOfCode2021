package day05

import scala.io.Source

val source = Source.fromResource("day05.in")
val input: List[Line] = source.getLines.map(line => {
    val Array(left, right) = line.split(" -> ")
    val Array(leftX, leftY) = left.split(",")
    val Array(rightX, rightY) = right.split(",")
    Line(Point(leftX.toInt, leftY.toInt), Point(rightX.toInt, rightY.toInt))
}).toList

final case class Point(x: Int, y: Int)
final case class Line(start: Point, end: Point)
type Grid = Map[Point, Int]
extension (grid: Grid)
    def paint(p: Point): Grid = grid.updatedWith(p) { case Some(count) => Some(count + 1); case None => Some(1); }
    def paint(x: Int, y: Int): Grid = paint(Point(x, y))

@main def main: Unit = {

    val result1 = {
        var grid: Grid = Map.empty.withDefault(_ => 0)
        for (Line(Point(lx, ly), Point(rx, ry)) <- input if lx == rx || ly == ry) {
            val xStep = Integer.compare(rx, lx)
            val yStep = Integer.compare(ry, ly)
            if (xStep != 0) {
                for (x <- lx to rx by xStep) {
                    grid = grid.paint(x, ly)
                }
            }
            if (yStep != 0) {
                for (y <- ly to ry by yStep) {
                    grid = grid.paint(lx, y)
                }
            }
        }
        grid.count { case (point, count) => count >= 2 }
    }
    println(result1)

    val result2 = {
        var grid: Grid = Map.empty.withDefault(_ => 0)
        for (Line(p1@Point(x1, y1), p2@Point(x2, y2)) <- input) {
            val (xStart, xEnd, xStep) = if x1 > x2 then (x2, x1, -1) else if x1 < x2 then (x1, x2, 1) else (x1, x2, 0)
            val (yStart, yEnd, yStep) = if y1 > y2 then (y2, y1, -1) else if y1 < y2 then (y1, y2, 1) else (y1, y2, 0)
            val points = LazyList.iterate(p1) { case Point(x, y) => Point(x + xStep, y + yStep) }.takeWhile(_ != p2).appended(p2)
            for (p <- points) {
                grid = grid.paint(p)
            }
        }
        grid.count { case (point, count) => count >= 2 }
    }
    println(result2) //911454 is too high, 19518 is not the right answer.

}