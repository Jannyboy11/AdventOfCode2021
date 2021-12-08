package day04

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ListBuffer

val source = Source.fromResource("day04.in")
val lines = source.getLines
val randomNumbers = { val numbers = lines.next().split(",").toList.map(_.toInt); lines.next(); /*skip empty line*/ numbers; }
val boards = {
    val bs = new ListBuffer[Board]()
    var board = new ListBuffer[List[Int]]()
    for (line <- lines) {
        if (line.isEmpty) {
            bs.addOne(board.toList)
            board = new ListBuffer[List[Int]]()
        } else {
            var numbers = line.split("\\s+").toList
            if (numbers.head.isEmpty) numbers = numbers.tail    //workaround the line starting with a space
            board.addOne(numbers.map(_.toInt))
        }
    }
    bs.addOne(board.toList) //add last board
    bs.toList
}

type Board = List[List[Int]]

@main def main: Unit = {

    val (result1, result2) = {
        var score1 = 0;
        var score2 = 0
        var draws: Set[Int] = Set[Int]()
        for (draw <- randomNumbers) {
            draws += draw
            boards.map(board => bingo(board, draws)).max match {
                case Some(sum) =>
                    val score = sum * draw
                    if (score > 0) {
                        if (score1 == 0) score1 = score
                        score2 = score
                    }
                case None => ()
            }
        }
        (score1, score2)
    }
    println(result1)
    println(result2)

}

def bingo(board: Board, draws: Set[Int]): Option[Int] =
    if (board.exists(_.forall(draws.contains)) || board.transpose.exists(_.forall(draws.contains)))
        Some(board.flatten.toSet.diff(draws).sum)
    else
        None
