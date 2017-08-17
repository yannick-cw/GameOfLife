package example

import org.scalatest._
import GolUI._
import GameOfLifeService._
import CellState._

class GameOfLifeServiceSpec extends FlatSpec with Matchers with OptionValues {

  it should "fulfill the first rule: Any live cell with fewer than two live neighbours dies, as if caused by underpopulation." in {
    calculateNextCellState(Alive, aliveNeighbours = 1) shouldEqual Dead
  }

  it should "fulfill the second rule: Any live cell with more than three live neighbours dies, as if by overcrowding." in {
    calculateNextCellState(Alive, aliveNeighbours = 4) shouldEqual Dead
  }

  it should "fulfill the third rule: Any live cell with two or three live neighbours lives on to the next generation." in {
    calculateNextCellState(Alive, aliveNeighbours = 2) shouldEqual Alive
    calculateNextCellState(Alive, aliveNeighbours = 3) shouldEqual Alive
  }

  it should "fulfill the fourth rule: Any dead cell with exactly three live neighbours becomes a live cell" in {
    calculateNextCellState(Dead, aliveNeighbours = 2) shouldEqual Dead
    calculateNextCellState(Dead, aliveNeighbours = 3) shouldEqual Alive
    calculateNextCellState(Dead, aliveNeighbours = 4) shouldEqual Dead
  }

  /*
  Generation 1:
  4 8
  ........
  ....*...
  ...**...
  ........
   */

  it should "create grid from ascii" in {
    val ascii = """........
                  |....*...
                  |...**...
                  |........""".stripMargin

    val expectedGrid = List(
      List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
    )

    parseAscii(ascii).value shouldEqual expectedGrid
  }

  it should "return None if an invalid grid is passed" in {
    val ascii = "invalid"

    parseAscii(ascii) shouldEqual None
  }

  it should "return None if grid has invalid dimensions" in {
      val ascii = """........
                    |....*..
                    |...**...
                    |........""".stripMargin

    parseAscii(ascii) shouldEqual None
  }

  it should "create ascii from grid" in {
    val grid = List(
      List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Alive, Alive, Dead, Dead, Dead),
      List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
    )

    val expectedAscii = """........
                          |....*...
                          |...**...
                          |........""".stripMargin

    renderAscii(grid) shouldEqual expectedAscii
  }

  it should "create a new generation" in {
    val grid = parseAscii("""........
                            |....*...
                            |...**...
                            |........""".stripMargin).get

    val expectedGrid = parseAscii("""........
                                     |...**...
                                     |...**...
                                     |........""".stripMargin).get

    createNextGen(grid) shouldEqual expectedGrid
  }
}
