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
    val newVelX =
        if probe.velX > 1 then probe.velX - 1
        else if probe.velX < 1 then probe.velX + 1
        else 0
    val newVelY = probe.velY - 1
    Probe(newPosX, newPosY, newVelX, newVelY)
}

def trajectory(probe: Probe, lowestAllowedY: Int): LazyList[Probe] =
    LazyList.iterate(probe)(step).takeWhile(p => p.posY >= lowestAllowedY)

enum Attempt:
    case Hit(maxPosY: Int)
    case Miss

def attempt(probe: Probe, target: Area): Attempt = {
    val path = trajectory(probe, target.minY)
    val Probe(posX, posY, _, _) = path.last

    if target.contains(posX, posY) then
        Attempt.Hit(path.map(_.posY).max)
    else
        Attempt.Miss
}

def go(target: Area): (Int, Int) = {
    var maxPosY = 0
    var count = 0

    for velX <- 0 to target.maxX; velY <- (target.minY-1) to 1000 do
        attempt(Probe(velX, velY), targetArea) match
            case Attempt.Hit(highestY) =>
                maxPosY = math.max(maxPosY, highestY)
                count += 1
            case Attempt.Miss => ()

    (maxPosY, count)
}

@main def main: Unit = {

    val (result1, result2) = go(targetArea)
    println(result1)
    println(result2)    //3545 too low? what am I doing wrong?

}