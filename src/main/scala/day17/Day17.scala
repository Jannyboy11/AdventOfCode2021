package day17

import scala.io.Source

val source = Source.fromResource("day17.in")
val targetArea: Area = source.getLines().next() match {
    case s"target area: x=$minX..$maxX, y=$minY..$maxY" => Area(minX.toInt, maxX.toInt, minY.toInt, maxY.toInt)
}

case class Area(minX: Int, maxX: Int, minY: Int, maxY: Int):
    def contains(posX: Int, posY: Int): Boolean =
        minX <= posX && posX <= maxX && minY <= posY && posY <= maxY

case class Probe(posX: Int, posY: Int, velX: Int, velY: Int)

object Probe:
    def apply(velX: Int, velY: Int): Probe = Probe(0, 0, velX, velY)

def step(probe: Probe): Probe = {
    val newPosX = probe.posX + probe.velX
    val newPosY = probe.posY + probe.velY
    val newVelX = if probe.velX > 0 then probe.velX - 1 else if probe.velX < 0 then probe.velX + 1 else 0
    val newVelY = probe.velY - 1
    Probe(newPosX, newPosY, newVelX, newVelY)
}

def simulate(probe: Probe, target: Area): Option[Int] = {
    var p = probe
    var maxY = 0
    while (p.posX <= target.maxX && p.posY >= target.minY) {
        maxY = Math.max(maxY, p.posY)
        if (target.contains(p.posX, p.posY))
            return Some(maxY)
        p = step(p)
    }
    None
}

def go(target: Area): (Int, Int) = {
    var maxPosY = 0
    var count = 0

    var velX = 0
    var velY = target.minY

    while (velX <= target.maxX) {
        while (velY <= 2000) {
            simulate(Probe(velX, velY), target) match
                case Some(highestY) =>
                    maxPosY = math.max(maxPosY, highestY)
                    count += 1
                case None =>
                    ()
            velY += 1
        }
        velX += 1
        velY = target.minY
    }

    (maxPosY, count)
}

@main def main: Unit = {

    val (result1, result2) = go(targetArea)
    println(result1)
    println(result2)

}