package day15

import scala.io.Source

val source = Source.fromResource("day15.in")
val grid: Grid[RiskLevel] = source.getLines().map(_.map(_ - '0').toIndexedSeq).toIndexedSeq

type RiskLevel = Int
type TotalRiskLevel = Int
type TotalRiskLevels = scala.collection.Map[Point, TotalRiskLevel]
type Grid[Elem] = IndexedSeq[IndexedSeq[Elem]]
case class Point(x: Int, y: Int)

extension [Elem] (grid: Grid[Elem])
    def apply(point: Point): Elem = grid(point.y)(point.x)
    def height = grid.length
    def width = grid.head.length
    def bottomRight: Point = Point(width - 1, height - 1)
    def updated(point: Point, value: Elem): Grid[Elem] = grid.updated(point.y, grid(point.y).updated(point.x, value))
    def fmap[B](f: Elem => B): Grid[B] = grid.map(_.map(f))

def adjacent(point: Point, maxX: Int, maxY: Int): Seq[Point] =
    Seq(
        Point(point.x-1, point.y), Point(point.x+1, point.y),
        Point(point.x, point.y-1), Point(point.x, point.y+1)
    ).filter { case Point(x, y) => 0 <= x && x <= maxX && 0 <= y && y <= maxY }

//adapted from https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Using_a_priority_queue
def dijkstra(source: Point, ownCosts: Grid[RiskLevel]): TotalRiskLevels = {
    val Point(maxX, maxY) = ownCosts.bottomRight

    val dist = scala.collection.mutable.Map.empty[Point, TotalRiskLevel].withDefaultValue(Integer.MAX_VALUE)
    val Q = new java.util.PriorityQueue[Point](java.util.Comparator.comparingInt[Point](p => dist(p)))

    dist.put(source, 0)
    Q.add(source)

    while !Q.isEmpty do
        val u = Q.poll()
        for v <- adjacent(u, maxX, maxY) do
            val alt = dist(u) + ownCosts(v) //distance from u to v is always equal to ownCosts(v)
            if alt < dist(v) then
                Q.remove(v)
                dist.put(v, alt)
                Q.add(v)

    dist
}

def concatRight[Elem](left: Grid[Elem], right: Grid[Elem]): Grid[Elem] =
    left.zip(right).map((row1, row2) => row1 ++ row2)

def concatBelow[Elem](upper: Grid[Elem], lower: Grid[Elem]): Grid[Elem] =
    upper ++ lower

def flattenGrid[Elem](g: Grid[Grid[Elem]]): Grid[Elem] =
    g.map(rowOfGrids => rowOfGrids.reduce(concatRight)).reduce(concatBelow)

@main def main: Unit = {

    val result1 = {
        val costs = dijkstra(Point(0, 0), grid)
        costs(grid.bottomRight)
    }
    println(result1)

    val result2 = {
        val newGrid = flattenGrid(IndexedSeq.tabulate(5, 5)((y, x) => grid.fmap(level => (level + x + y - 1) % 9 + 1)))
        val costs = dijkstra(Point(0, 0), newGrid)
        costs(newGrid.bottomRight)
    }
    println(result2)

}