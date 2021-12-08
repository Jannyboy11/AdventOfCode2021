package day03

import scala.io.Source

val source = Source.fromResource("day03.in")
val input = source.getLines.toList.map(_.toList)

@main def main: Unit = {

    val transposed = input.transpose
    val len = transposed.size

    val result1 = {
        val gammaChars = new Array[Char](len)
        for (i <- 0 until len) {
            val counts = count(transposed(i));
            gammaChars(i) = counts.maxBy(_._2)._1
        }
        val epsilonChars = gammaChars.map(inverse)
        val gamma = Integer.parseUnsignedInt(gammaChars.mkString, 2)
        val epsilon = Integer.parseUnsignedInt(epsilonChars.mkString, 2);
        gamma * epsilon
    }
    println(result1)

    val result2 = {
        var remainingOxygen = input
        var remainingCO2 = input
        var i = 0
        while (i < len && (remainingOxygen.length > 1 || remainingCO2.length > 1)) {
            if (remainingOxygen.length > 1) {
                val oxygenChar = mostCommon(count(remainingOxygen.transpose.apply(i)))
                remainingOxygen = remainingOxygen.filter(_.apply(i) == oxygenChar)
            }
            if (remainingCO2.length > 1) {
                val co2Char = leastCommon(count(remainingCO2.transpose.apply(i)))
                remainingCO2 = remainingCO2.filter(_.apply(i) == co2Char)
            }
            i += 1
        }
        val oxygen = Integer.parseUnsignedInt(remainingOxygen.head.mkString, 2)
        val co2 = Integer.parseUnsignedInt(remainingCO2.head.mkString, 2)
        oxygen * co2
    }
    println(result2)

}

def mostCommon(counts: Map[Char, Int]): Char =
    if (counts('0') > counts('1')) '0' else '1'

def leastCommon(counts: Map[Char, Int]): Char =
    if (counts('1') < counts('0')) '1' else '0'

def inverse(c: Char): Char = c match {
    case '0' => '1'
    case '1' => '0'
}

def count[A](input: Iterable[A]): Map[A, Int] = {
    var result = Map[A, Int]()
    for (a <- input) {
        result = result.updatedWith(a) {
            case Some(count) => Some(count + 1)
            case None => Some(1)
        }
    }
    result
}