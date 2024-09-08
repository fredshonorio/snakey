package snakey

sealed trait Input
object Input {
  case object clockwise        extends Input
  case object counterclockwise extends Input
  case object left             extends Input
  case object right            extends Input
  case object up               extends Input
  case object down             extends Input
}

case class Controls(binds: Map[Int, Input]) {
  def parse(key: Int): Option[Input] = binds.get(key)
}

object Controls {
  val space     = 32
  val backspace = 127
  val tab       = 9
  val enter     = 13

  val up    = 65
  val down  = 66
  val left  = 68
  val right = 67
}

sealed trait Direction {
  def change(i: Input): Direction =
    this match {
      case Direction.left =>
        i match {
          case Input.clockwise        => Direction.up
          case Input.counterclockwise => Direction.down
          case Input.left             => Direction.left
          case Input.right            => Direction.left
          case Input.up               => Direction.up
          case Input.down             => Direction.down
        }
      case Direction.right =>
        i match {
          case Input.clockwise        => Direction.down
          case Input.counterclockwise => Direction.up

          case Input.left  => Direction.right
          case Input.right => Direction.right
          case Input.up    => Direction.up
          case Input.down  => Direction.down
        }

      case Direction.up =>
        i match {
          case Input.clockwise        => Direction.right
          case Input.counterclockwise => Direction.left

          case Input.left  => Direction.left
          case Input.right => Direction.right
          case Input.up    => Direction.up
          case Input.down  => Direction.up
        }

      case Direction.down =>
        i match {
          case Input.clockwise        => Direction.left
          case Input.counterclockwise => Direction.right

          case Input.left  => Direction.left
          case Input.right => Direction.right
          case Input.up    => Direction.down
          case Input.down  => Direction.down
        }

    }
}

object Direction {
  case object left  extends Direction
  case object right extends Direction
  case object up    extends Direction
  case object down  extends Direction
}
