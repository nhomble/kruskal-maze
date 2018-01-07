package org.hombro

import scala.collection.mutable

package object maze {

  // maze parameters: we specify number of cells in horizontal and vertical direction
  // as well as the physical height and width of the maze
  case class MazeParams(nx: Int, ny: Int, width: Double, height: Double)

  // we'll idetify cells by their integer ids
  type CellID = Int
  // Set label
  type Label = Int
  // we represent side of a cell as an ordered tuple of two integers (c1,c2)
  // where c1 and c2 are cell ids and c1 < c2
  type Side = (Int, Int)
  type Coord = (Double, Double)

  def generateEdges(params: MazeParams): List[Side] = {
    //vertical edges
    val vEdges = for {
      i <- 0 to params.nx - 2
      j <- 0 to params.ny - 1
      left = i + params.nx * j
      right = left + 1
    } yield (left, right)

    //horizontal edges
    val hEdges = for {
      i <- 0 to params.nx - 1
      j <- 0 to params.ny - 2
      bottom = i + params.nx * j
      top = i + params.nx * (j + 1)
    } yield (bottom, top)

    vEdges.toList ++ hEdges.toList
  }

  def removeEdges(params: MazeParams, edges: List[Side]): List[Side] = {
    val cellLabels: Map[CellID, Label] = (0 until params.nx * params.ny).map(id => (id, id)).toMap
    val cellSets: Map[Label, Set[CellID]] = Map()
    val shuffled = util.Random.shuffle(edges)

    def remove(input: List[Side],
               res: List[Side],
               labels: Map[CellID, Label],
               sets: Map[Label, Set[CellID]]): List[Side] = {
      input match {
        case (c1, c2) :: t =>
          val l1 = labels.getOrElse(c1, c1)
          val l2 = labels.getOrElse(c2, c2)
          //println(s"considering edge ${(c1,c2)}, labels are ${(l1,l2)}")
          if (l1 == l2)
          //if cells are in the same set we keep the edge
            remove(t, (c1, c2) :: res, labels, sets)
          else {
            val s1 = sets.getOrElse(l1, Set(c1))
            val s2 = sets.getOrElse(l2, Set(c2))
            val (from, to) = if (s1.size < s2.size) (l1, l2) else (l2, l1)
            //merge cell sets
            val newSet = s1.union(s2)
            //update labels
            val newLabels = labels.map { case (k, v) => if (v == from) (k, to) else (k, v) }
            //println(s"labels are different we will update $from to $to, new map is $newLabels, new set is $newSet")
            remove(t, res, newLabels, sets.updated(to, newSet))
          }
        case Nil => res
      }
    }

    remove(shuffled, Nil, cellLabels, cellSets)
  }

  def removeEdgesMut(params: MazeParams, edges: List[Side]): List[Side] = {
    val cellLabels: mutable.Map[CellID, Label] = mutable.Map((0 until params.nx * params.ny).map(id => (id, id)): _*)
    val cellSets: mutable.Map[Label, Set[CellID]] = mutable.Map()
    val shuffled = util.Random.shuffle(edges)
    var res: List[Side] = Nil
    shuffled.foreach {
      case (c1, c2) =>
        val l1 = cellLabels.getOrElse(c1, c1)
        val l2 = cellLabels.getOrElse(c2, c2)
        if (l1 == l2)
          res = (c1, c2) :: res
        else {
          val s1 = cellSets.getOrElse(l1, Set(c1))
          val s2 = cellSets.getOrElse(l2, Set(c2))
          val (toLabel, fromSet) = if (s1.size < s2.size) (l2, s1) else (l1, s2)
          //merge cell sets
          cellSets(toLabel) = s1.union(s2)
          //update labels
          println(s"updating ${fromSet.size} labels")
          fromSet.foreach(id => cellLabels(id) = toLabel)
        }
    }
    res
  }

  def getRenderingInfo(params: MazeParams, edges: List[Side]): List[(Coord, Coord)] = {
    val (nx, ny) = (params.nx, params.ny)
    val dh = params.width / nx
    val dv = params.height / ny
    edges.map {
      case (c1, c2) =>
        val (i1, j1) = (c1 % nx, c1 / nx)
        val (i2, j2) = (c2 % nx, c2 / nx)
        (i1 == i2, j1 == j2) match {
          //horizontal edge
          case (true, false) =>
            val (left_x, right_x) = (i1 * dh, (i1 + 1) * dh)
            val y = (j1 + 1) * dv
            ((left_x, y), (right_x, y))
          //vertical edge
          case (false, true) =>
            val (bottom_y, top_y) = (j1 * dv, (j1 + 1) * dv)
            val x = (i1 + 1) * dh
            ((x, bottom_y), (x, top_y))
          case _ => throw new RuntimeException("Poop")
        }
    }
  }

  def generateKruskal(params: MazeParams): List[(Coord, Coord)] = {
    val edges = generateEdges(params)
    val remaining = removeEdgesMut(params, edges)
    getRenderingInfo(params, remaining)
  }
}

