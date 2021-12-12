package day12

import scala.io.Source

val source = Source.fromResource("day12.in")
val input = source.getLines().map { line =>
    val Array(from, to) = line.split("-")
    Edge(from, to)
} .toList

type Node = String
case class Edge(from: Node, to: Node)
type Graph = Map[Node, Set[Node]]

val graph = {
    var g: Graph = Map()
    for (Edge(from, to) <- input) {
        g = g.updatedWith(from) {
            case Some(set) => Some(set + to)
            case None => Some(Set(to))
        }
        g = g.updatedWith(to) {
            case Some(set) => Some(set + from)
            case None => Some(Set(from))
        }
    }
    g
}

def isSmall(node: Node): Boolean = node.charAt(0).isLower

case class Path private(val path: List[Node], private val seen: Set[Node], private val markedSmallCave: Option[Node]):
    def +(node: Node): Path = new Path(node :: path, if isSmall(node) then seen + node else seen, markedSmallCave)
    def seen(node: Node): Boolean = seen.contains(node)
    def *(node: Node): Set[Path] =
        if (isSmall(node)) {
            if ("end".equals(node) || "start".equals(node)) {
                Set(new Path(node :: path, seen + node, markedSmallCave))
            } else {
                markedSmallCave match
                    case None if !seen.contains(node) => Set(
                        new Path(node :: path, seen, Some(node)), //mark it, but don't add it
                        new Path(node :: path, seen + node, None), //do add it, but don't mark it
                    )
                    case None => Set(new Path(node :: path, seen, Some(node))) //it was already seen, so mark it
                    case Some(n) if !seen.contains(node) => Set(new Path(node :: path, seen + node, Some(n))) //it was already marked, but not seen
                    case Some(n) => Set() //out of options
            }
        } else {
            Set(new Path(node :: path, seen, markedSmallCave))
        }
    override def toString: String = s"Path(nodes=${path.reverse.mkString("[", "," , "]")},special=${markedSmallCave})"

object Path:
    def apply(node: Node): Path = new Path(List(node), Set(node), None)

def explore1(graph: Graph, from: Node, currentPath: Path, paths: Set[Path]): Set[Path] = {
    var ps = paths
    for (to <- graph(from)) {
        val smallCave = isSmall(to)
        if (smallCave && currentPath.seen(to)) {
            //dead end -- continue
        } else if (to == "end") {
            ps += (currentPath + to)
        } else {
            val p = explore1(graph, to, currentPath + to, ps)
            ps ++= p
        }
    }
    ps
}

def explore2(graph: Graph, from: Node, currentPath: Path, paths: Set[Path]): Set[Path] = {
    var ps = paths
    for (to <- graph(from)) {
        val smallCave = isSmall(to)
        if (smallCave && currentPath.seen(to)) {
            //dead end -- continue
        } else if (to == "end") {
            ps ++= (currentPath * to)
        } else {
            val newPaths = currentPath * to
            for (newPath <- newPaths) {
                val p = explore2(graph, to, newPath, ps)
                ps ++= p
            }
        }
    }
    ps
}

@main def main: Unit = {

    val result1 = explore1(graph, "start", Path("start"), Set()).size
    println(result1)

    val result2 = explore2(graph, "start", Path("start"), Set()).map(_.path).size
    println(result2)

}