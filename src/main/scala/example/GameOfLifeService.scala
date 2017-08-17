package example

import scala.util.Try

object GameOfLifeService {

  type CellGrid = List[List[CellState]]

  def createNextGen(grid: CellGrid): CellGrid =
    grid.par
      .map(_.zipWithIndex)
      .zipWithIndex
      .map {
        case (row, rowIndex) =>
          row.par.map {
            case (cell, columnIndex) => calculateNextCellState(cell, aliveNeighbors(grid)(rowIndex, columnIndex))
          }.toList
      }
      .toList

  private val getCellState: CellGrid => (Int, Int) => CellState = grid =>
    (rowIndex, columnIndex) =>
      (for {
        row  <- Try(grid(rowIndex))
        cell <- Try(row(columnIndex))
      } yield cell).getOrElse(Dead)

  private val aliveNeighbors: CellGrid => (Int, Int) => Int = grid =>
    (rowIndex, columnIndex) =>
      (for {
        rowOffset    <- -1 to 1
        columnOffset <- -1 to 1
        if rowOffset != 0 || columnOffset != 0
      } yield getCellState(grid)(rowOffset + rowIndex, columnOffset + columnIndex)).collect { case Alive => 1 }.sum

  private[example] def calculateNextCellState(cellState: CellState, aliveNeighbours: Int): CellState = {
    (cellState, aliveNeighbours) match {
      case (Dead, 3)  => Alive
      case (Alive, 2) => Alive
      case (Alive, 3) => Alive
      case _          => Dead
    }
  }

}
