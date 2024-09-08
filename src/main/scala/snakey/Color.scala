package snakey

sealed trait Color {
  def hex: String = this match {
    case Color.red    => "#ff0000"
    case Color.green  => "#00ff00"
    case Color.yellow => "#ffff00"
    case Color.black  => "#000000"
  }
}
object Color {
  case object red    extends Color
  case object yellow extends Color
  case object green  extends Color
  case object black  extends Color
}
