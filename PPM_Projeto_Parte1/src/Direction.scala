import UtilsGameEngine.Coord2D

object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West,
  NorthEast, NorthWest, SouthEast, SouthWest,Invalid = Value


  def nextPos(direction: Direction, pos: Coord2D):Coord2D = direction match{
    case North => (pos._1, pos._2 - 1)
    case South => (pos._1, pos._2 + 1)
    case East => (pos._1 + 1, pos._2)
    case West => (pos._1 - 1, pos._2)
    case NorthEast => (pos._1 + 1, pos._2 - 1)
    case NorthWest => (pos._1 - 1, pos._2 - 1)
    case SouthEast => (pos._1 + 1, pos._2 + 1)
    case SouthWest => (pos._1 - 1, pos._2 + 1)
  }

  def getDirection(iCoord: Coord2D, fCoord: Coord2D):Direction = {
    val dX = iCoord._1 - fCoord._1
    val dY = iCoord._2 - fCoord._2

    (dX, dY) match{
    case (0,1)  => North
    case (0,-1) => South
    case (-1,0)  => East
    case (1,0) => West
    case (-1,1) => NorthEast
    case (1,1) => NorthWest
    case (-1,-1) => SouthEast
    case (1,-1) => SouthWest
    case _ => Invalid
  }
  }
}
