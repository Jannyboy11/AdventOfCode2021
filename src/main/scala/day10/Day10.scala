package day10

import scala.annotation.tailrec
import scala.io.Source

val source = Source.fromResource("day10.in")
val input = source.getLines().toSeq.map(_.toList.asInstanceOf[Line])

type Open = '(' | '[' | '{' | '<'
type Close = ')' | ']' | '}' | '>'
type Brace = Open | Close
type Line = List[Brace]
type Expect = List[Close]

def closing(open: Open): Close = open match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
}

def corruptPoints(close: Close): Int = close match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
}

def completePoints(close: Close): Int = close match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
}

def completionScore(expect: Expect): Long =
    expect.foldLeft(0L) { case (total: Long, close: Close) => total * 5 + completePoints(close) }

def step(line: Line, expect: Expect): Either[Int, (Line, Expect)] = (line, expect) match {
    case (Nil, ex) => Right((Nil, ex))
    case (hl :: tl, he :: te) if hl == he => Right((tl, te))
    case ((hl: Open) :: tl, ex) => Right((tl, closing(hl) :: ex))
    case ((corrupt: Close) :: _, _) => Left(corruptPoints(corrupt))
}

@tailrec def go1(line: Line, expect: Expect): Int = step(line, expect) match {
    case Right((Nil, _)) => 0
    case Right((remaining, expected)) => go1(remaining, expected)
    case Left(points) => points
}

@tailrec def go2(line: Line, expect: Expect): Long = step(line, expect) match {
    case Right((Nil, expect)) => completionScore(expect)
    case Right((remaining, expected)) => go2(remaining, expected)
}

@main def main: Unit = {

    val result1 = input.map(line => go1(line, Nil)).sum
    println(result1)

    val result2 = {
        val scores = input.filter(line => go1(line, Nil) == 0)
            .map(line => go2(line, Nil))
            .sorted
        scores(scores.length / 2)
    }
    println(result2)

}