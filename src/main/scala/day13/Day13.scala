package day13;

import scala.io.Source

val source = Source.fromResource("day13.in")
val lines = source.getLines()
val paper: Paper = lines.takeWhile(_.nonEmpty).map(row => {
    val Array(x, y) = row.split(",")
    Point(x.toInt, y.toInt)
}).toSet

val pattern = """fold along ([xy])=(\d+)""".r("axis", "at")
val folds: List[Fold] = lines.map[Fold] {
    case pattern("x", at) => FoldX(at.toInt)
    case pattern("y", at) => FoldY(at.toInt)
}.toList

case class Point(x: Int, y: Int)
case class FoldX(x: Int)
case class FoldY(y: Int)

type Paper = Set[Point]
type Fold = FoldX | FoldY

def foldAlongX(at: Int, paper: Paper): Paper = paper.map {
    case p@Point(x, y) => if x > at then Point(at - (x - at), y) else p
}

def foldAlongY(at: Int, paper: Paper): Paper = paper.map {
    case p@Point(x, y) => if y > at then Point(x, at - (y - at)) else p
}

def fold(paper: Paper, fold: Fold): Paper = fold match {
    case FoldX(at) => foldAlongX(at, paper)
    case FoldY(at) => foldAlongY(at, paper)
}

@main def main: Unit = {

    val result1 = fold(paper, folds.head).size
    println(result1)

    val foldedPaper = folds.foldLeft(paper)(fold)
    val (maxX, maxY) = foldedPaper.foldLeft((0, 0)) { case ((mX, mY), Point(x, y)) => (Math.max(mX, x), Math.max(mY, y)) }
    for (y <- 0 to maxY) {
        for (x <- 0 to maxX)
            if foldedPaper(Point(x, y)) then print("##") else print("  ")
        println()
    }
    //val result2 = "ARHZPCUH"

}
