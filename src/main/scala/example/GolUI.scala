package example

import example.GameOfLifeService.CellGrid
import cats.implicits._

object GolUI {

  def renderAscii(grid: CellGrid): String =
    grid
      .map(row => row.map(CellState.toChar).mkString)
      .mkString("\n")

  def parseAscii(ascii: String): Option[CellGrid] = {

    val maybeGrid: Option[List[List[CellState]]] = ascii
      .split("\n")
      .toList
      .traverse(_.toCharArray.toList.traverse(CellState.fromChar))

    if (hasValidDimensions(maybeGrid)) maybeGrid else None
  }

  private def hasValidDimensions(maybeGrid: Option[List[List[CellState]]]): Boolean =
    (for {
      grid <- maybeGrid
      head <- grid.headOption
    } yield grid.forall(row => row.length == head.length)).getOrElse(false)

}
