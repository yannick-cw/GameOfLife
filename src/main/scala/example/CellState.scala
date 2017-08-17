package example
import scala.io.AnsiColor._

trait CellState
case object Alive extends CellState
case object Dead  extends CellState
object CellState {
  def toChar(cellState: CellState): String = cellState match {
    case Alive => BLUE + "*" + RESET
    case Dead  => "."
  }

  def fromChar(c: Char): Option[CellState] = c match {
    case '.' => Some(Dead)
    case '*' => Some(Alive)
    case _   => None
  }
}
