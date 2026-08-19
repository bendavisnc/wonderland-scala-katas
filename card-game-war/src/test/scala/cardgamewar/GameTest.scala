package cardgamewar

import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*

class GameTest extends munit.FunSuite {

  test("play round, the highest rank wins the cards in the round") {
    assertEquals(playRound(cards.head, cards.last), Some(cards.last))
  }

  test("play round, queens are higher rank than jacks") {
    assertEquals(playRound(Card(Queen, Heart), Card(Jack, Heart)), Some(Card(Queen, Heart)))
  }

  test("play round, kings are higher rank than queens") {
    assertEquals(playRound(Card(King, Heart), Card(Queen, Heart)), Some(Card(King, Heart)))
  }

  test("play round, aces are higher rank than kings") {
    assertEquals(playRound(Card(Ace, Heart), Card(King, Heart)), Some(Card(Ace, Heart)))
  }

  test("play game, the player loses when they run out of cards") {
    val (playerOneCards, playerTwoCards) =
      cards.zipWithIndex.foldLeft((Seq.empty[Card], Seq.empty[Card])) {
        case ((cardsA, cardsB), (card, i)) if i % 2 == 1 => (cardsA.appended(card), cardsB)
        case ((cardsA, cardsB), (card, i)) => (cardsA, cardsB.appended(card))
      }
    assertEquals((playerOneCards.size, playerTwoCards.size), (26, 26))
    val result = playGame(playerOneCards, playerTwoCards)
    assertEquals(result.playerOneCards.size, 52)
    assertEquals(result.playerTwoCards.size, 0)
    assertEquals(result.winner, PlayerOne)
  }
}
