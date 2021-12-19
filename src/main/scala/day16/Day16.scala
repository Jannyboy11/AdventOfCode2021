package day16

import java.util.Formattable
import scala.io.Source

val source = Source.fromResource("day16.in")
val input = source.getLines().next()
val inputBits: Bits = input.flatMap[Bit]((c: Char) => bits(c))

type Bit = 0 | 1
type Bits = Seq[Bit]

def bits(hexadeimal: Char): Bits = {
    val number = Integer.parseUnsignedInt(Character.toString(hexadeimal), 16)
    val bitString = String.format("%4s", Integer.toBinaryString(number)).replace(' ', '0')
    bitString.map[Bit](c => (c - '0').asInstanceOf[Bit])
}

def toInt(bits: Bits): Int = bits.foldRight((1, 0)) { case (bit, (base, total)) => (base * 2, total + base * bit) } ._2
def toLong(bits: Bits): Long = bits.foldRight((1L, 0L)) { case (bit, (base, total)) => (base * 2, total + base * bit) } ._2

trait Decoder[A]:
    def decode(bits: Bits): (A, Bits)

object Decoder {
    val version: Decoder[Version] = bits => {
        val (vBits, rest) = bits.splitAt(3)
        (toInt(vBits), rest)
    }

    val typeId: Decoder[TypeId] = bits => {
        val (idBits, rest) = bits.splitAt(3)
        val typeId = toInt(idBits) match
            case 4 => TypeId.Literal
            case 0 => TypeId.Sum
            case 1 => TypeId.Product
            case 2 => TypeId.Minimum
            case 3 => TypeId.Maximum
            case 5 => TypeId.GreaterThen
            case 6 => TypeId.LessThen
            case 7 => TypeId.EqualTo
            case _ => TypeId.UnknownOperator
        (typeId, rest)
    }

    val literalValue: Decoder[Packet.LiteralValue] = bits => {
        def parse(bits: Bits): (Bits, Bits) = bits match
            case Seq(1, a, b, c, d, remainder: _*) =>
                val chunk = Seq(a, b, c, d).asInstanceOf[Bits]
                val (otherChunks, rest) = parse(remainder.asInstanceOf[Bits])
                (chunk ++ otherChunks, rest)
            case Seq(0, a, b, c, d, remainder: _*) =>
                (Seq(a, b, c, d).asInstanceOf[Bits], remainder.asInstanceOf[Bits])

        val (numberBits, remainder) = parse(bits)
        (Packet.LiteralValue(toLong(numberBits)), remainder)
    }

    val operatorMode: Decoder[OperatorMode] = bits => bits.head match {
        case 0 => (OperatorMode.CountBits, bits.tail)
        case 1 => (OperatorMode.CountPackets, bits.tail)
    }

    def operatorLength(operatorMode: OperatorMode): Decoder[Int] = bits => operatorMode match {
        case OperatorMode.CountBits =>
            val (lengthBits, remainder) = bits.splitAt(15)
            (toInt(lengthBits), remainder)
        case OperatorMode.CountPackets =>
            val (lengthBits, remainder) = bits.splitAt(11)
            (toInt(lengthBits), remainder)
    }

    val operator: Decoder[Packet.Operator] = bits => {
        val (mode, r1) = operatorMode.decode(bits)
        val (length, r2) = operatorLength(mode).decode(r1)
        val (arguments, r3) = operands(mode, length).decode(r2)
        (Packet.Operator(mode, length, arguments), r3)
    }

    def layerContents(typeId: TypeId): Decoder[Seq[Packet]] = bits => typeId match {
        case TypeId.Literal =>
            val (pLiteralValue, remainder) = literalValue.decode(bits)
            (Seq(pLiteralValue), remainder)
        case _ =>
            val (pOperator, remainder) = operator.decode(bits)
            (Seq(pOperator), remainder)
    }

    val layer: Decoder[Packet.Layer] = bits => {
        val (ver, r1) = version.decode(bits)
        val (tid, r2) = typeId.decode(r1)
        val (pacs, r3) = layerContents(tid).decode(r2)
        (Packet.Layer(ver, tid, pacs), r3)
    }

    def operands(mode: OperatorMode, length: Int): Decoder[Seq[Packet]] = bits => mode match {
        case OperatorMode.CountBits =>
            val (operandBits, r1) = bits.splitAt(length)
            var remainingBits = operandBits
            val seqBuilder = Seq.newBuilder[Packet]
            while remainingBits.nonEmpty do
                val (pak, bitsAfterPacket) = layer.decode(remainingBits)
                seqBuilder += pak
                remainingBits = bitsAfterPacket
            (seqBuilder.result, r1)
        case OperatorMode.CountPackets =>
            val seqBuilder = new scala.collection.mutable.ListBuffer[Packet]()
            var remainingBits = bits
            while seqBuilder.length < length do
                val (pak, bitsAfterPacket) = layer.decode(remainingBits)
                seqBuilder += pak
                remainingBits = bitsAfterPacket
            (seqBuilder.toSeq, remainingBits)
    }

}

type Version = Int

enum TypeId:
    case Literal
    case Sum
    case Product
    case Minimum
    case Maximum
    case GreaterThen
    case LessThen
    case EqualTo
    case UnknownOperator

enum OperatorMode:
    case CountBits
    case CountPackets

enum Packet:
    case Layer(version: Version, typeId: TypeId, contents: Seq[Packet])
    case LiteralValue(number: Long)
    case Operator(mode: OperatorMode, length: Int, operands: Seq[Packet])

def eval1(packet: Packet): Int = packet match {
    case Packet.Layer(version, _, contents) => version + contents.map(eval1).sum
    case Packet.Operator(_, _, operands)    => operands.map(eval1).sum
    case _                                  => 0
}

def eval2(packet: Packet): Long = packet match {
    case Packet.Layer(_, TypeId.Literal, Seq(Packet.LiteralValue(number))) => number
    case Packet.Layer(_, operatorId, Seq(Packet.Operator(_, _, operands))) =>
        def f: Seq[Long] => Long = operatorId match
            case TypeId.Sum => _.sum
            case TypeId.Product => _.product
            case TypeId.Minimum => _.min
            case TypeId.Maximum => _.max
            case TypeId.GreaterThen => values => if values(0) > values(1) then 1L else 0L
            case TypeId.LessThen => values => if values(0) < values(1) then 1L else 0L
            case TypeId.EqualTo => values => if values(0) == values(1) then 1L else 0L
        f(operands.map(eval2))

}

@main def main: Unit = {

    val (packet, remainder) = Decoder.layer.decode(inputBits)
    val result1 = eval1(packet)
    println(result1)

    val result2 = eval2(packet)
    println(result2)

}