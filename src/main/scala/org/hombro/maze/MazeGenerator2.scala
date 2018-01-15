package org.hombro.maze

import scala.collection.mutable

object MazeGenerator2 {

  private def removeEdges(edges: List[Side])(implicit params:MazeParams): List[Side] = {
    val cellLabels: mutable.Map[CellID, Label] =
      mutable.Map((0 until params.nx * params.ny).map(id => (id, id)): _*)
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

  def generate(implicit params: MazeParams): RefEdgeList = {
    val edges = generateAllEdges
    val remaining = removeEdges(edges)
    getReferenceRenderingInfo(remaining)
  }
}
