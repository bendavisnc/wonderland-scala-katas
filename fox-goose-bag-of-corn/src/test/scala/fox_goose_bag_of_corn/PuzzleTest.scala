package fox_goose_bag_of_corn

import Puzzle.*

class PuzzleTest extends munit.FunSuite {

  def validateMove(step1: RiverCrossingSection, step2: RiverCrossingSection): RiverCrossingSection =
    val symmetricDiff = step1.diff(step2).union(step2.diff(step1))
    assert(symmetricDiff.size < 3)
    if symmetricDiff.size > 0 then {
      assert(symmetricDiff.contains(🤷))
    }
    step2

  test("river crossing plan") {
    val crossingPlan = riverCrossingPlan()
    // you begin with the fox, goose and corn on one side of the river
    assertEquals(List(Set(🦊, 🪿, 🌽, 🤷), Set(⛵), Set()), crossingPlan.head)
    // you end with the fox, goose and corn on one side of the river
    assertEquals(List(Set(), Set(⛵), Set(🦊, 🪿, 🌽, 🤷)), crossingPlan.last)
    // things are safe
    val leftBank  = crossingPlan.map(_.head)
    val rightBank = crossingPlan.map(_.last)
    // the fox and the goose should never be left alone together
    assert(leftBank.concat(rightBank).forall(_ != Set(🦊, 🪿)))
    // the goose and the corn should never be left alone together
    assert(leftBank.concat(rightBank).forall(_ != Set(🪿, 🌽)))
    // The boat can carry only you plus one other
    val boatPositions = crossingPlan.map(_.tail.head)
    assert(boatPositions.forall(_.size <= 3))
    // moves are valid
    val leftMoves   = crossingPlan.map(_.head)
    val middleMoves = crossingPlan.map(_.tail.head)
    val rightMoves  = crossingPlan.map(_.last)
    leftMoves.reduce(validateMove)
    middleMoves.reduce(validateMove)
    rightMoves.reduce(validateMove)
  }

}
