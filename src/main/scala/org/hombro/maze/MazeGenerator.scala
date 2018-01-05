package org.hombro.maze

import scala.collection.mutable

object MazeGenerator {
  def create(x: Int, y: Int): List[Edge] = {
    val board = List.tabulate(x, y)((x, y) => Vertex(x, y))
    val vertices: List[Vertex] = board.flatten

    val edges = (for (v <- vertices) yield {
      List(
        board.lift(v.x - 1).flatMap(row => row.lift(v.y)),
        board.lift(v.x + 1).flatMap(row => row.lift(v.y)),
        board.lift(v.x).flatMap(row => row.lift(v.y - 1)),
        board.lift(v.x).flatMap(row => row.lift(v.y + 1))
      ).flatten.map(other => Edge(v, other))
    }).flatten
    val sets = vertices.map(v => v -> Set[Vertex](v)).toMap
    println(s"Total edges ${edges.size}")
    kruskal(mutable.Map() ++ sets, edges)
  }

  /**
    * Hardly a proper kruskal since I'm being stupid lazy instead of properly doing a disjoint set.
    */
  private def kruskal(sets: mutable.Map[Vertex, Set[Vertex]], edges: List[Edge]) = {
    val openings = mutable.ListBuffer[Edge]()
    val shuffled = util.Random.shuffle(edges)

    for (e <- shuffled) yield {
      val s1 = sets(e.u)
      val s2 = sets(e.v)
      if (s1.intersect(s2).isEmpty) {
        val rez = s1.union(s2)
        for (c <- rez) yield {
          sets(c) = rez
        }
        openings.append(e)
      }
    }
    openings.toList
  }
}

sealed trait Direction {}

case class North() extends Direction {}

case class South() extends Direction {}

case class West() extends Direction {}

case class East() extends Direction {}

case class Vertex(x: Int, y: Int) {
  def direction(other: Vertex): Direction = {
    if (x > other.x) West()
    else if (x < other.x) East()
    else if (y > other.y) North()
    else if (y < other.y) South()
    else
      throw new RuntimeException("poopy")
  }
}

case class Edge(u: Vertex, v: Vertex) {
  def opposite = Edge(v, u)

  def vertices = List(u, v)

  /**
    * Helper method to combine to the two squares into a rectangular hallway
    */
  def tunnel(length: Int): (Int, Int, Int, Int) = {
    u.direction(v) match {
      case South() => opposite.tunnel(length)
      case West() => opposite.tunnel(length)
      case North() => (v.x, v.y, length, 2 * length)
      case East() => (u.x, u.y, 2 * length, length)
    }
  }
}
