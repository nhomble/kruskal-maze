package org.hombro.maze

import scala.collection.mutable

object MazeGenerator3 {
  type Move = (CellID, CellID)
  type Wavefront = mutable.Set[Move]
  type SpanningTree = mutable.Set[CellID]

  // given a cell takes 4 of its neighbors and filters out those neighbor cells that are either
  // already in a spanning tree or beyond the boundaries of the domain
  private def cellNeighbors(st: SpanningTree, c: CellID)(implicit params: MazeParams): List[CellID] = {
    val (i, j) = Util.cellIDToCoord(c)
    // TODO: probably all the filtering can be done at once at the expense of readability
    List((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)).filter {
      case (x, y) => x < params.nx && x >= 0 && y < params.ny && y >= 0
    }.map(Util.coordToCellID).filter(!st.contains(_))
  }

  // pick random move from a wavefront, add the new cell to the spanning tree, updte wavefront
  // and return the removed edge
  // TODO: think about using Option[Move] here
  private def makeAMove(wf: Wavefront, st: SpanningTree)(implicit params: MazeParams): Move = {
    val move = randomSetElement(wf)
    val moveTo = move._2
    //add new cell to the spanning tree
    st += moveTo
    //remove the move(s) from a wavefront(there could be more than one move that leads to the new cell
    wf.retain { case (_, to) => to != moveTo }
    //compute wavefront update
    val newMoves = cellNeighbors(st, moveTo).map(neighbor => (moveTo, neighbor))
    wf ++= newMoves
    move
  }

  def generate(implicit params: MazeParams): RefEdgeList = {
    // initialize
    // spanning tree contains one cell and wavefront contains cell neighbors
    val numCells = params.nx * params.ny
    val startingCell = util.Random.nextInt(numCells)
    val st: SpanningTree = mutable.Set(startingCell)
    val wf: Wavefront = mutable.Set(
      cellNeighbors(st, startingCell).map(n => (startingCell, n)): _*
    )
    var moves: List[Move] = Nil
    while (wf.nonEmpty)
      moves = makeAMove(wf, st) :: moves
    getReferenceRenderingInfo(getEdgeSetComplement(moves))
  }
}
