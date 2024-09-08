package snakey

import cats.effect.IO
import fs2.io.file.Path

trait Keyboard {
  // translate coordinates into the keycode used by kontroll
  def translate(x: Int, y: Int): Int
  def width: Int
  def height: Int
  def dim: Dim = Dim(width, height)
}

object Keyboard {
  // 0  1  2  3  4  5  | 26 27 28 29 30 31
  // 6  7  8  9  10 11 | 32 33 34 35 36 37
  // 12 13 14 15 16 17 | 38 39 40 41 42 43
  // 18 19 20 21 22 23 | 44 45 46 47 48 49
  //             24 25 | 50 51
  val voyager: Keyboard = new Keyboard {
    override def translate(x: Int, y: Int): Int =
      if (x < 0 || x >= width || y < 0 || y > height) 0
      else {
        val colOffset = if (x >= 6) x + 20 else x
        val rowOffset = y * 6

        colOffset + rowOffset
      }

    override def width: Int  = 12
    override def height: Int = 4

  }
}

trait Render {
  def restore: IO[Unit]
  def set(k: Int, color: Color): IO[Unit]
}

object Render {
  def voyager(kontroll: Path): Render = new Render {
    import scala.sys.process._
    private val executable                           = kontroll.toString
    private def exec(args: String*): IO[Unit]        = IO.blocking(Process(args.prepended(executable)).!!).void
    override def restore: IO[Unit]                   = exec("restore-rgb-leds")
    override def set(k: Int, color: Color): IO[Unit] = exec("set-rgb", "-l", k.toString, "-c", color.hex)
  }
}

case class InputBuffer(inputs: Vector[Input]) {
  private val max                    = 3
  def put(input: Input): InputBuffer = InputBuffer(inputs.appended(input).takeRight(max))

  def uncons: Option[(Input, Vector[Input])] = inputs.headOption match {
    case Some(head) => Some(head -> inputs.tail)
    case None       => None
  }
}

object InputBuffer {
  def empty: InputBuffer = InputBuffer(Vector())
}
