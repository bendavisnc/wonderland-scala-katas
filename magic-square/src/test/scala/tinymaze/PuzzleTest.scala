package magicsquare

import magicsquare.Puzzle.*

class PuzzleTest extends munit.FunSuite {

  def sumRows(m: Square): Seq[Double] = m.map(_.reduce(_ + _))

  // format: off
  def sumColumns(m: Square): Seq[Double] =
    Seq(m.map(_.head).reduce(_ + _),
        m.map(_.apply(1)).reduce(_ + _),
        m.map(_.head).reduce(_ + _))
  // format: on


  // format: off
  def sumDiagonals(m: Square): Seq[Double] =
    Seq(m(0)(0) + m(1)(1) + m(2)(2),
        m(2)(0) + m(1)(1) + m(0)(2))
  // format: on

  test("all the rows, columns, and diagonal add to the same number") {
    assert(sumRows(magicSquare(values)).toSet == sumColumns(magicSquare(values)).toSet)
    assert(sumColumns(magicSquare(values)).toSet == sumDiagonals(magicSquare(values)).toSet)

    assertEquals(Seq(sumRows(magicSquare(values)).toSet,
                     sumColumns(magicSquare(values)).toSet,
                     sumDiagonals(magicSquare(values)).toSet
                 ).toSet.size,
                 1
    )
  }
}
