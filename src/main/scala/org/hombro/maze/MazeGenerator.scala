package org.hombro.maze

import scala.collection.mutable

object MazeGenerator {
  def create(x: Int, y: Int): List[Edge] = {
    val board = List.tabulate(x, y)((x, y) => new Vertex(x, y))
    val vertices: List[Vertex] = board.flatten
    val edges = (for (v <- vertices) yield {
      List(
        if (v.x > 0) board.lift(v.x - 1).get.lift(v.y) else None,
        if (v.x < x - 1) board.lift(v.x + 1).get.lift(v.y) else None,
        if (v.y > 0) board.lift(v.x).get.lift(v.y - 1) else None,
        if (v.y < y - 1) board.lift(v.x).get.lift(v.y + 1) else None
      ).flatten.map(other => new Edge(v, other))
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
        for(c <- rez) yield {
          sets(c) = rez
        }
        openings.append(e)
      }
    }
    println(s"openings ${openings.length}")
    openings.toList
  }
}

object Direction extends Enumeration {
  val NORTH: Value = Value
  val SOUTH: Value = Value
  val EAST: Value = Value
  val WEST: Value = Value
}

class Vertex(val x: Int, val y: Int) {
  def direction(other: Vertex): Direction.Value = {
    if (x > other.x) Direction.WEST
    else if (x < other.x) Direction.EAST
    else if (y > other.y) Direction.NORTH
    else if (y < other.y) Direction.SOUTH
    else
      throw new RuntimeException("poopy")
  }

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[Vertex])
      false
    else {
      val other = obj.asInstanceOf[Vertex]
      x == other.x && y == other.y
    }
  }

  override def hashCode(): Int = x.hashCode() ^ y.hashCode() + 37

  override def toString = s"($x, $y)"
}

class Edge(val u: Vertex, val v: Vertex) {
  def opposite = new Edge(v, u)

  def vertices = List(u, v)

  /**
    * Helper method to combine to the two squares into a rectangular hallway
    */
  def tunnel(length: Int): (Int, Int, Int, Int) = {
    val d = u.direction(v)
    if (d.equals(Direction.SOUTH) || d.equals(Direction.WEST)) {
      opposite.tunnel(length)
    }
    else if (d.equals(Direction.NORTH)) {
      (v.x, v.y, length, 2 * length)
    }
    else if (d.equals(Direction.EAST)) {
      (u.x, u.y, 2 * length, length)
    }
    else {
      throw new RuntimeException("poop")
    }
  }

  override def toString = s"${u.toString} : ${v.toString}"
}
