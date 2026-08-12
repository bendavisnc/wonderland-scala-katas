package tinymaze

class SolverTest extends munit.FunSuite {

  // test("mazeAssoc") {
  //   val maze: Maze     = Seq(Seq(S, O, I), Seq(I, O, I), Seq(I, O, E))
  //   val expected: Maze = Seq(Seq(S, O, I), Seq(I, O, I), Seq(I, O, X))
  //   assertEquals(mazeAssoc(maze, (2, 2), X), expected)
  // }

  test("can find way to exit with 3x3 maze") {
    // format: off
    val maze: Maze = Seq(Seq(S, O, I),
                         Seq(I, O, I),
                         Seq(I, O, E))

    val solution: Maze = Seq(Seq(X, X, I),
                             Seq(I, X, I),
                             Seq(I, X, X))

    // format: on
    assertEquals(Solver.solveMaze(maze), solution)

  }

  test("can find way to exit with 4x4 maze") {
    // format: off
    val maze: Maze = Seq(Seq(S, O, O, I),
                         Seq(I, I, O, O),
                         Seq(I, O, O, I),
                         Seq(I, I, O, E))

    val solution: Maze = Seq(Seq(X, X, X, I),
                             Seq(I, I, X, O),
                             Seq(I, O, X, I),
                             Seq(I, I, X, X))

    // format: on
    assertEquals(Solver.solveMaze(maze), solution)

  }

}
