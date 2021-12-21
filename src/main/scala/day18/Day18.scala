package day18

import scala.io.Source

val source = Source.fromResource("day18.in")
val input: Seq[Flat.SnailfishNumber] = source.getLines().map(Flat.parse).toSeq

object Flat {

    enum C {
        case Open
        case Number(value: Int)
        case Close
    }

    type SnailfishNumber = Seq[C]

    extension (base: SnailfishNumber) {
        def replace(at: Index, `with`: C): SnailfishNumber = {
            val (left, right) = base.splitAt(at)
            left ++ Seq(`with`) ++ right.tail
        }
        def insert(at: Index, inBetween: SnailfishNumber): SnailfishNumber = {
            val (left, right) = base.splitAt(at)
            left ++ inBetween ++ right
        }
        def removed(at: Index, length: Int): SnailfishNumber = {
            val (left, right) = base.splitAt(at)
            left ++ right.drop(length)
        }
    }

    def parse(string: String): SnailfishNumber = string.flatMap[C] {
        case '[' => Seq(C.Open)
        case ']' => Seq(C.Close)
        case ',' => Seq()
        case d if d.isDigit => Seq(C.Number(d - '0'))
    }

    type Index = Int

    def reduce(sfn: SnailfishNumber): SnailfishNumber = {
        findExplodePoint(sfn) match {
            case Some(explodePoint) => return reduce(explode(explodePoint, sfn))
            case None => ()
        }
        findSplitPoint(sfn) match {
            case Some(splitPoint) => return reduce(split(splitPoint, sfn))
            case None => ()
        }
        sfn
    }

    def findSplitPoint(sfn: SnailfishNumber): Option[Index] = {
        val index = sfn.indexWhere { case C.Number(x) if x >= 10 => true; case _ => false }
        if index == -1 then None else Some(index)
    }

    def split(splitPoint: Index, base: SnailfishNumber): SnailfishNumber = {
        val C.Number(n) = base(splitPoint)
        val x = n / 2
        val y = n - x
        val replacement = Seq(C.Open, C.Number(x), C.Number(y), C.Close)
        base
            .removed(splitPoint, 1)
            .insert(splitPoint, replacement)
    }

    def findExplodePoint(sfn: SnailfishNumber): Option[Index] = {
        var index: Index = 0
        var depth: Int = 0
        for (c <- sfn) {
            c match {
                case C.Open => depth += 1
                case C.Close => depth -= 1
                case digit => ()
            }
            if (depth > 4) {
                return Some(index)
            }
            index += 1
        }
        None
    }

    def explode(explodePoint: Index, base: SnailfishNumber): SnailfishNumber = {
        var result = base

        val leftIndex = result.lastIndexWhere(_.isInstanceOf[C.Number], explodePoint)
        if (leftIndex != -1) {
            val C.Number(left) = base(leftIndex)
            val C.Number(x) = base(explodePoint + 1)
            result = result.replace(leftIndex, C.Number(x + left))
        }

        val rightIndex = result.indexWhere(_.isInstanceOf[C.Number], explodePoint + 3)
        if (rightIndex != -1) {
            val C.Number(right) = base(rightIndex)
            val C.Number(y) = base(explodePoint + 2)
            result = result.replace(rightIndex, C.Number(y + right))
        }

        result.removed(explodePoint, 4)
            .insert(explodePoint, Seq(C.Number(0)))
    }

    def add(one: SnailfishNumber, two: SnailfishNumber): SnailfishNumber = {
        reduce(Seq(C.Open) ++ one ++ two ++ Seq(C.Close))
    }
}

object Tree {

    enum SFN {
        case Digit(value: Int)
        case Pair(x: SFN, y: SFN)
    }

    def fromFlat(flat: Flat.SnailfishNumber): SFN = {
        import Flat.*
        import SFN.*

        def reify(flat: Flat.SnailfishNumber): Seq[C | ','] = flat match {
            case Seq(C.Number(x), C.Open, r0: _*) => Seq[C | ','](C.Number(x), ',', C.Open) ++ reify(r0)
            case Seq(C.Number(x), C.Number(y), r0: _*) => Seq[C | ','](C.Number(x), ',', C.Number(y)) ++ reify(r0)
            case Seq(C.Close, C.Number(y), r0: _*) => Seq[C | ','](C.Close, ',', C.Number(y)) ++ reify(r0)
            case Seq(C.Close, C.Open, r0: _*) => Seq[C | ','](C.Close, ',', C.Open) ++ reify(r0)
            case Seq(h, t: _*) => h +: reify(t)
            case Seq() => Seq[C | ',']()
        }

        //IntelliJ thinks these casts are redundant, but they are not!
        def parse(input: Seq[C | ',']): (SFN, Seq[C | ',']) = input match {
            case Seq(C.Number(x), r0: _*) => (Digit(x), r0.asInstanceOf[Seq[C | ',']])
            case Seq(C.Open, r0: _*) =>
                val (x, Seq(',', r1: _*)) = parse(r0.asInstanceOf[Seq[C | ',']])
                val (y, Seq(C.Close, r2: _*)) = parse(r1.asInstanceOf[Seq[C | ',']])
                (Pair(x, y), r2.asInstanceOf[Seq[C | ',']])
        }

        val (res, Nil) = parse(reify(flat))
        res
    }

    def magnitude(snf: SFN): Int = snf match {
        case SFN.Digit(value) => value
        case SFN.Pair(x, y) => 3 * magnitude(x) + 2 * magnitude(y)
    }
}

@main def main: Unit = {

    val result1 = Tree.magnitude(Tree.fromFlat(input.reduceLeft(Flat.add)))
    println(result1)

    val result2 = {
        var max = 0
        for (snf1 <- input) {
            for (snf2 <- input) {
                if (snf1 != snf2) {
                    val magnitude = Tree.magnitude(Tree.fromFlat(Flat.add(snf1, snf2)))
                    max = Math.max(max, magnitude)
                }
            }
        }
        max
    }
    println(result2)

}