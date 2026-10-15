package tinymaze

import cats.syntax.all.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Solver {

  def nextMazeSteps(mazeStep: MazeFirstStep): MazeSteps =
    Seq(
      MazeStep(maze = mazeStep.maze.set((0, 0), X),
               position = mazeStep.position,
               mazeSize = mazeStep.mazeSize
      )
    )

  def nextMazeSteps(mazeStep: MazeStep): MazeSteps =
    Seq[Direction](Left, Right, Up, Down).flatMap { direction =>
      for
        nextPosition <- Some(direction |+| mazeStep.position)
        if nextPosition.within(mazeStep.mazeSize)
        mazeElement <- mazeStep.maze.get(nextPosition)

        step = mazeStep.copy(
          position = nextPosition,
          maze = mazeStep.maze.set(nextPosition, X)
        )

        result <- mazeElement match
          case O => Some(step)
          case E => Some(MazeLastStep(step))
          case _ => None
      yield result
    }

  def search(maze: Maze): MazeSteps =
    @tailrec
    def loop(queue: Queue[MazeSteps]): MazeSteps =
      val mazeSteps = queue.head
      mazeSteps.last match {
        case _: MazeLastStep           => mazeSteps
        case latestMazeStep: IMazeStep =>
          loop(
            queue.tail.appendedAll(
              (latestMazeStep match
                case s: MazeStep      => nextMazeSteps(s)
                case s: MazeFirstStep => nextMazeSteps(s)
                case _: MazeLastStep  => ???
              ).map(mazeSteps.appended(_))
            )
          )
      }

    loop(Queue(Seq(MazeFirstStep(maze))))

  def solveMaze(maze: Maze): Maze =
    search(maze).lastOption match {
      case None => throw new Exception(s"Failed to obtain maze solution, `$maze`.")
      case Some(mazeStep: IMazeStep) => mazeStep.maze
    }

}
