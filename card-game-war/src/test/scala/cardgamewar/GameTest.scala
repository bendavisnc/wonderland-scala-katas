package cardgamewar

class GameTest extends munit.FunSuite {

  test("play round, the highest rank wins the cards in the round") {
    assertEquals(2, 3)
  }

  test("play round, queens are higher rank than jacks") {
    assertEquals(2, 3)
  }

  test("play round, kings are higher rank than queens") {
    assertEquals(2, 3)
  }

  test("play round, aces are higher rank than kings") {
    assertEquals(2, 3)
  }

  test("play game, the player loses when they run out of cards") {
    assertEquals(2, 3)
  }
}
