package snakey
import cats.effect._
import cats.effect.std.Random
import cats.syntax.all._
import fs2.io.file.Path
import fs2.Stream
import Controls._
import org.jline.terminal.TerminalBuilder
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Cfg {
  val controls = Controls(
    Map(
      backspace -> Input.left,
      enter     -> Input.right,
      space     -> Input.up,
      tab       -> Input.down,
      left      -> Input.left,
      right     -> Input.right,
      up        -> Input.up,
      down      -> Input.down,
    )
  )

  val keeb   = Keyboard.voyager
  val render = Render.voyager(Path("/home/fred/kontroll"))
}

object Main extends IOApp.Simple {

  import Cfg._
  override def run: IO[Unit] = {

    val fps      = 7L
    val tickrate = 1.second / fps

    render.restore >> (
      IO.ref(Game.build(keeb, controls, Rng.empty)),
      Random.scalaUtilRandom[IO]
    ).flatMapN { (state, rand) =>
      implicit val r: Random[IO] = rand
      inputLoop(state).mergeHaltBoth(renderLoop(tickrate, keeb, render, state)).compile.drain
    }
  }

  private def inputLoop(state: Ref[IO, Game]): Stream[IO, Unit] = {
    val initTerm = IO(TerminalBuilder.terminal()).flatTap(t => IO(t.enterRawMode()))
    val term     = Resource.make(initTerm)(t => IO.blocking(t.close()))
    val reads    = Stream.resource(term.map(_.reader())).flatMap(r => Stream.repeatEval(IO.blocking(r.read())))

    reads.evalTap(input => state.update(_.acceptInput(input))).void
  }

  private def renderLoop(
    rate: FiniteDuration,
    kb: Keyboard,
    render: Render,
    game: Ref[IO, Game],
  )(implicit rand: Random[IO]): Stream[IO, Unit] = {
    def renderDiff(prev: Board)(curr: Board): IO[Unit] =
      prev.diff(curr).traverse_ { case (Coord(x, y), color) => render.set(kb.translate(x, y), color) }

    val initialFrame = Board.build(kb.width, kb.height)
    Stream
      .fixedRate[IO](rate)
      // keep the previous frame so that we only change the pixels that are different
      .evalScan(initialFrame) { (prev, _) =>
        game.get
          .flatMap(Rng.generate) // randomness is generated before every frame, keeping the rendering pure
          // conceptually there's a possible race-condition because 'game' is read twice, but the read for the rng
          // only cares about the snake and the pellets, which are only changed in the tick
          .flatMap(rng => game.modify(_.tick(rng)).flatTap(renderDiff(prev)))
      }
      .void
  }
}
