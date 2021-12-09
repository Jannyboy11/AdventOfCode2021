package day06

import scala.collection.immutable.IntMap
import scala.io.Source

val source = Source.fromResource("day06.in")
val input: List[LanternFish] = source.getLines().next().split(",").map(_.toInt).toList

type LanternFish = Int
def step(fish: LanternFish): List[LanternFish] = fish match {
    case 0 => List(6, 8)
    case t => List(t-1)
}

def step(fishes: Map[LanternFish, BigInt]): Map[LanternFish, BigInt] = {
    val updated = fishes.map { case (k, v) => (k-1, v) }
    val reproduced = updated.getOrElse(-1, BigInt(0))
    val refreshed = updated.updatedWith(6) { case Some(amount) => Some(amount + reproduced); case None => Some(reproduced); }
    val withNew = refreshed.updated(8, reproduced)
    withNew.removed(-1)
}

@main def main: Unit = {

    val result1 = {
        var fishes = input
        for (_ <- 0 until 80) {
            fishes = fishes.flatMap(step)
        }
        fishes.size
    }
    println(result1)

    val result2 = {
        var fishes: Map[LanternFish, BigInt] = input.foldLeft[Map[LanternFish, BigInt]](IntMap.empty.withDefaultValue(0)) { case (acc, fish) => acc.updatedWith(fish) {
            case Some(count) => Some(count + BigInt(1))
            case None => Some(BigInt(1))
        }}
        for (_ <- 0 until 256) {
            fishes = step(fishes)
        }
        fishes.values.sum
    }
    println(result2)

}