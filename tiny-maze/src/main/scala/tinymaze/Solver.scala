package tinymaze

import scala.collection.immutable.Queue

object Solver {

  def nextMazeSteps(mazeStep: MazeFirstStep): MazeSteps =
    Seq(
      MazeStep(maze = mazeAssoc(mazeStep.maze, (0, 0), X),
               position = mazeStep.position,
               mazeSize = mazeStep.mazeSize
      )
    )

  def nextMazeSteps(mazeStep: MazeStep): MazeSteps =
    Seq[Direction](Left, Right, Up, Down).flatMap(direction => {
      val position   = (mazeStep.position._1 + direction._1, mazeStep.position._2 + direction._2)
      val shouldMark = Some(O) == mazeStep.maze.lift(position._2).flatMap(_.lift(position._1))
      val atEnd      = Some(E) == mazeStep.maze.lift(position._2).flatMap(_.lift(position._1))
      if position._1 < mazeStep.mazeSize._1 && position._2 < mazeStep.mazeSize._2 && (shouldMark || atEnd)
      then
        if atEnd then
          Seq(
            MazeLastStep(maze = mazeAssoc(mazeStep.maze, position, X),
                         position = position,
                         mazeSize = mazeStep.mazeSize
            )
          )
        else Seq(mazeStep.copy(maze = mazeAssoc(mazeStep.maze, position, X), position = position))
      else Seq.empty
    })

  def search(maze: Maze): MazeSteps =
    def loop(queue: Queue[MazeSteps]): MazeSteps =
      val mazeSteps = queue.head
      mazeSteps.last match {
        case MazeLastStep(_, _, _)     => mazeSteps
        case latestMazeStep: IMazeStep =>
          loop(
            queue.tail.appendedAll(
              (latestMazeStep match
                case s: MazeStep      => nextMazeSteps(s)
                case s: MazeFirstStep => nextMazeSteps(s)
                case s: MazeLastStep  => ???
              ).map(nextMazeStep => mazeSteps.appended(nextMazeStep))
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
