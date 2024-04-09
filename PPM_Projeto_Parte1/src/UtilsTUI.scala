import Direction.{Direction, East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
import Main.{Board, Coord2D, iterateBoard}

import scala.io.StdIn.readLine

object UtilsTUI {

  def showPrompt(): Unit = {
    print("\n(T)ry word or (q)uit: ")
  }

  def printBoard(board: Board):Unit = {
    def printChar(c:Char, p:Coord2D):Char = {
      if(p._1 == 0){System.out.print("\n")}
      System.out.print(c.toString + " ")
      c
    }
    iterateBoard(board, printChar)
  }

  def getUserInput(): String = readLine.trim.toUpperCase

  def stringToDirection(s: String): Direction = s match {
    case "North" => Direction.North
    case "South" => Direction.South
    case "East" => Direction.East
    case "West" => Direction.West
    case "NorthEast" => Direction.NorthEast
    case "NorthWest" => Direction.NorthWest
    case "SouthEast" => Direction.SouthEast
    case "SouthWest" => Direction.SouthWest
  }

  def stringToPoint(s: String): Coord2D = {
    val Array(x, y) = s.split(",").map(_.toInt)
    (x, y)
  }

  def askForWord(): (String, Coord2D, Direction.Value) = {

    println("\nTry (word:initialCoordinate:direction)")

    val Array(word, pointStr, directionStr) = readLine().split(":")
    val point = stringToPoint(pointStr)
    val direction = stringToDirection(directionStr)

    (word, point, direction)

  }

  def printGameState(gameState: GameState): Unit = {
    printBoard(gameState.board._1)
    println("\n Words Remainig: " + gameState.WordsRemainig)
  }



  def printGameOver(): Unit = println("\n=== GAME OVER ===")

  def printNewGame(): Unit = println("\n=== NEW GAME ===")


}
