import Direction._
import UtilsGameEngine.{Board, Coord2D, getItem, interactWithBoard, iterateBoard}

import scala.io.StdIn.{readInt, readLine}

object UtilsTUI {
  val RESET = "\u001B[0m"
  val PROMPT_MESSAGE = "\n(T)ry word or (q)uit: "
  val INVALID_KEY_MESSAGE = "Invalid Key"
  val WORD_INPUT_MESSAGE = "\nWhat word do you want to input"
  val COORDINATES_INPUT_MESSAGE = "\nWhat are the coordinates (line,column)"
  val DIRECTION_INPUT_MESSAGE = "\nWhat is the initial direction? (Ex. North, SouthEast, West)"
  val GAME_OVER_MESSAGE = "\n=== GAME OVER ==="
  val VICTORY_MESSAGE = "\n=== CONGRATULATIONS ==="
  val ASK_BOARD_SIZE = "\nWhat is the size of the board you want to play with?"

  def showPrompt(): Unit = {
    print(PROMPT_MESSAGE)
  }

  def askBoardSize(): Int = {
    print(ASK_BOARD_SIZE)
    val input = readLine()
    try {
      input.toInt
    } catch {
      case _: NumberFormatException =>
        println("Invalid input. Please enter a valid number.")
        askBoardSize() // Recursive call
    }
  }

  def printBoard(board: Board, colorBoard: Board): Unit = {
    def getColor(c: Char): String = c match {
      case 'W' => "\u001B[0m"
      case 'G' => "\u001B[32m"
    }

    def printChar(c: Char, p: Coord2D): Char = {
      val colorType = getItem(getItem(colorBoard, p._2), p._1)
      val color = getColor(colorType)
      if (p._1 == 0) {
        System.out.print("\n")
      }
      System.out.print(color + c.toString + RESET + " ")
      c
    }

    interactWithBoard(board, printChar)
  }

  def wordToColor(w: List[Char], c: String): String = w match {
    case Nil => ""
    case head :: tail => wordToColor(tail, c) + c
  }

  def getUserInput(): String = readLine.trim.toUpperCase

  def stringToDirection(s: String): Direction = s.toUpperCase match {
    case "NORTH" => NORTH
    case "SOUTH" => SOUTH
    case "EAST" => EAST
    case "WEST" => WEST
    case "NORTHEAST" => NORTHEAST
    case "NORTHWEST" => NORTHWEST
    case "SOUTHEAST" => SOUTHEAST
    case "SOUTHWEST" => SOUTHWEST
    case _ => throw new IllegalArgumentException("Invalid direction.")
  }

  def stringToPoint(s: String): Coord2D = {
    val Array(x, y) = s.split(",").map(_.toInt)
    (x, y)
  }

  def askForPlay(): (String, Coord2D, Direction) = {
    (askWord(), askPoint(), askDirection())
  }

  def askWord(): String = {
    println(WORD_INPUT_MESSAGE)
    readLine().toUpperCase()
  }

  def askPoint(): Coord2D = {
    println(COORDINATES_INPUT_MESSAGE)
    val input = readLine()
    try {
      stringToPoint(input)
    } catch {
      case _: NumberFormatException | _: ArrayIndexOutOfBoundsException =>
        println("Invalid input format. Please enter coordinates in the format 'line,column'.")
        askPoint() // Recursive call
    }
  }

  def askDirection(): Direction = {
    println(DIRECTION_INPUT_MESSAGE)
    val input = readLine().toUpperCase()
    try {
      stringToDirection(input)
    } catch {
      case _: IllegalArgumentException =>
        println("Invalid direction. Please enter a valid direction.")
        askDirection() // Recursive call
    }
  }

  def printGameState(gameState: GameState): Unit = {
    printBoard(gameState.board._1, gameState.colorBoard)
    println("\n Words Remaining: " + gameState.wordsToFind.length)
  }

  def printGameOver(): Unit = println(GAME_OVER_MESSAGE)

  def printVictory(): Unit = println(VICTORY_MESSAGE)
}
