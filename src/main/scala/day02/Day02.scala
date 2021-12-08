package day02

import scala.io.Source

val source = Source.fromResource("day02.in")
val input = source.getLines.map(line => {
    val Array(direction, amount) = line.split(" ")
    Instruction(Direction.parse(direction), amount.toInt)
}).toList

enum Direction:
    case Forward extends Direction
    case Down extends Direction
    case Up extends Direction

object Direction:
    def parse(string: String): Direction = string match {
        case "forward" => Forward
        case "down" => Down
        case "up" => Up
    }

case class Instruction(direction: Direction, amount: Int)

@main def main: Unit = {
    import Direction.{Forward, Down, Up}

    val result1 = {
        var horizontal = 0
        var vertical = 0
        for (Instruction(direction, amount) <- input) {
            direction match {
                case Forward => horizontal += amount
                case Down => vertical -= amount
                case Up => vertical += amount
            }
        }
        horizontal * -vertical
    }
    println(result1)

    val result2 = {
        var aim = 0;
        var depth = 0;
        var horizontal = 0;
        for (Instruction(direction, amount) <- input) {
            direction match {
                case Direction.Down => aim += amount
                case Direction.Up => aim -= amount
                case Direction.Forward =>
                    horizontal += amount
                    depth += aim * amount
            }
        }
        horizontal * depth
    }
    println(result2)

}

