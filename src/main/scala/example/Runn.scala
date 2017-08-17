package example

import java.util.concurrent.TimeUnit

import GolUI._
import example.GameOfLifeService.CellGrid
import GameOfLifeService._

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.Random

object Run extends App {

  val grid = parseAscii("""...*....
                          |..***...
                          |...*....
                          |........""".stripMargin).get

  def getDeadOrAlive: CellState =
    if (Random.nextDouble() > 0.3) Dead else Alive

  val bigGrid = List.fill(50)(List.fill(200)(getDeadOrAlive))

  println(renderAscii(bigGrid))

  @tailrec
  def next(grid: CellGrid, time: Long = System.nanoTime()): CellGrid = {
    val nextGen = createNextGen(grid)
    println()
    println(renderAscii(nextGen))
    val now = System.nanoTime()
    println(s"time passed ${Duration.create(now - time, TimeUnit.NANOSECONDS).toMillis}ms")
    Console.flush()
    next(nextGen, now)
  }

  next(bigGrid)

}
