package org.hombro

import scala.collection.mutable

package object maze {

  // maze parameters: we specify number of cells in horizontal and vertical direction
  // as well as the physical height and width of the maze
  case class MazeParams(nx: Int, ny: Int)

  //helper functions to transform between cell coordinates and cell IDs
  object Util {
    //given cell coordinates, return cell id
    def coordToCellID(coord: Coord)(implicit params: MazeParams): CellID = coord._1 + coord._2 * params.nx

    //given cell id return it's coordinates
    def cellIDToCoord(id: CellID)(implicit params: MazeParams): Coord = (id % params.nx, id / params.nx)
  }

  // we'll idetify cells by their integer ids
  type CellID = Int
  // Set label
  type Label = Int
  // we represent side of a cell as an ordered tuple of two integers (c1,c2)
  // where c1 and c2 are cell ids and c1 < c2
  // TODO: for side ordering requirement we need to somehow enforce the order stated above
  type Side = (CellID, CellID)
  type Coord = (Int, Int)
  type RefEdgeList = List[(Coord, Coord)]

  trait Sortable[A]{
    def sort:A
  }

  implicit def sortableIntTuple(s:Side) = new Sortable[Side] { def sort:Side = if(s._1 < s._2) (s._1,s._2) else (s._2, s._1)}

  // TODO: perhaps need to make this safer with an Option[A] return type
  def randomSetElement[A](s: mutable.Set[A]): A = {
    val n = util.Random.nextInt(s.size)
    s.iterator.drop(n).next
  }

  // generates all the edges in a mesh, except for the boundary ones
  // TODO: can probably be optimized
  def generateAllEdges(implicit params: MazeParams): List[Side] = {
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
  // given a set of edges and maze parameters, returns the complementary set of edges
  // original set + complementary set = all edges
  def getEdgeSetComplement(edges:List[Side])(implicit params: MazeParams):List[Side] =
    (generateAllEdges.toSet diff edges.map(_.sort).toSet).toList

  // generates edge endpoints in the reference coordinate system
  // where bottom left cell has coordinates (0,0) and (1,1) for bottom left and top right
  // vertices correspondingly
  def getReferenceRenderingInfo(edges: List[Side])(implicit params: MazeParams): RefEdgeList = {
    val (nx, ny) = (params.nx, params.ny)
    edges.map {
      side =>
        //make sure cells are sorted to save on match branches below
        val (c1, c2) = side.sort
        val (i1, j1) = Util.cellIDToCoord(c1)
        val (i2, j2) = Util.cellIDToCoord(c2)
        (i1 == i2, j1 == j2) match {
          //horizontal edge
          case (true, false) =>
            val (left_x, right_x) = (i1, i1 + 1)
            val y = j1 + 1
            ((left_x, y), (right_x, y))
          //vertical edge
          case (false, true) =>
            val (bottom_y, top_y) = (j1, j1 + 1)
            val x = i1 + 1
            ((x, bottom_y), (x, top_y))
          case _ => throw new RuntimeException("Poop")
        }
    }
  }
}

