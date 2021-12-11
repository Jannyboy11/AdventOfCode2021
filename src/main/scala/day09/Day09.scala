package day09;

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day09.in")
val input: Array[Array[Int]] = source.getLines().map((s: String) => s.map((c: Char) => c - '0').toArray).toArray

extension (map: Array[Array[Int]])
    def apply(y: Int, x: Int): Int = map(y)(x)

def riskLevel(lowPoint: Int): Int = lowPoint + 1

def lowPoint(y: Int, x: Int, map: Array[Array[Int]]): Option[Int] = {
    val heigth = map.length
    val width = map(0).length
    val adjecents = Seq((y-1, x), (y+1, x), (y, x-1), (y,x+1)).filter((y, x) => 0 <= y && y < heigth && 0 <= x && x < width)
    val value = map(y, x)
    Option.when(adjecents.forall((y, x) => map(y, x) > value))(value)
}

def higherPoints(y: Int, x: Int, map: Array[Array[Int]]): Set[(Int, Int)] = {
    val heigth = map.length
    val width = map(0).length
    val adjecents = Seq((y-1, x), (y+1, x), (y, x-1), (y,x+1)).filter((y, x) => 0 <= y && y < heigth && 0 <= x && x < width)
    val value = map(y, x)
    adjecents.toSet.filter((y, x) => { val higherValue = map(y, x); higherValue > value && higherValue < 9 })
}


@main def main: Unit = {

    val riskLevels = new mutable.ListBuffer[Int]()
    val lowPoints: mutable.Set[(Int, Int)] = mutable.Set.empty
    for (y <- input.indices) {
        for (x <- input(0).indices) {
            lowPoint(y, x, input) match {
                case Some(point) =>
                    riskLevels.addOne(riskLevel(point))
                    lowPoints.add((y, x))
                case None =>
            }
        }
    }
    val result1 = riskLevels.sum
    println(result1)

    val basins: mutable.Map[(Int, Int), mutable.Set[(Int, Int)]] = new mutable.HashMap[(Int, Int), mutable.Set[(Int, Int)]]()
    def explore(lowPoint: (Int, Int)): Unit = {
        basins.put(lowPoint, mutable.Set(lowPoint))
        def exp(point: (Int, Int)): Unit = {
            basins(lowPoint).add(point)
            for (highPoint@(hY, hX) <- higherPoints(point._1, point._2, input)) {
                exp(hY, hX)
            }
        }
        higherPoints(lowPoint._1, lowPoint._2, input).foreach(exp)
    }
    lowPoints.foreach(explore)
    val result2 = basins.values.toList.sortBy(-_.size).take(3).map(_.size).product
    println(result2)

}