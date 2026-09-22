package doublets

import Solver.*

class SolverTest extends munit.FunSuite {

  test("with word links found") {
    assertEquals(doublets("head", "tail"), Seq("head", "heal", "teal", "tell", "tall", "tail"))
    assertEquals(doublets("door", "lock"), Seq("door", "boor", "book", "look", "lock"))
    assertEquals(doublets("bank", "loan"), Seq("bank", "bonk", "book", "look", "loon", "loan"))
    assertEquals(doublets("wheat", "bread"),
                 Seq("wheat", "cheat", "cheap", "cheep", "creep", "creed", "breed", "bread")
    )
  }

  test("with no word links found") {
    assertEquals(doublets("ye", "freezer"), Seq.empty)
  }

}
