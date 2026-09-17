package cardgamewar

import cardgamewar.*
import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cats.data.State
import cats.data.StateT

class GameTest extends munit.FunSuite {

  test("play round, the highest rank wins the cards in the round") {
    assertEquals(
      playRound(cards.head, cards.last),
      cards.last
    )
  }

  test("play round, queens are higher rank than jacks") {
    assertEquals(
      playRound((Queen, ♥), (Jack, ♥)),
      (Queen, ♥): Card
    )
  }

  test("play round, kings are higher rank than queens") {
    assertEquals(
      playRound((King, ♥), (Queen, ♥)),
      (King, ♥): Card
    )
  }

  test("play round, aces are higher rank than kings") {
    assertEquals(
      playRound((Ace, ♥), (King, ♥)),
      (Ace, ♥): Card
    )
  }

  test("play game, the player loses when they run out of cards, unless hands dealt are mirrored") {
    val (playerOneCards, playerTwoCards) =
      cards.zipWithIndex.foldLeft((Seq.empty[Card], Seq.empty[Card])) {
        case ((cardsA, cardsB), (card, i)) if i % 2 == 1 => (cardsA.appended(card), cardsB)
        case ((cardsA, cardsB), (card, i)) => (cardsA, cardsB.appended(card))
      }
    assertEquals((playerOneCards.size, playerTwoCards.size), (26, 26))
    val (cardHands, _) = playGame(drawCards)
      .run(
        Map(PlayerOne -> playerOneCards, PlayerTwo -> playerTwoCards)
      )
      .value

    assertEquals(cardHands.get(PlayerOne).toSeq.flatten.size, 0)
    assertEquals(cardHands.get(PlayerTwo).toSeq.flatten.size, 0)
    assertEquals(cardHands.get(Discard).toSeq.flatten.size, 52)
  }

  test("play game, the player loses when they run out of cards") {
    val (cardsA, cardsB) =
      cards.zipWithIndex.foldLeft((Seq.empty[Card], Seq.empty[Card])) {
        case ((cardsA, cardsB), (card, i)) if i % 2 == 1 => (cardsA.appended(card), cardsB)
        case ((cardsA, cardsB), (card, i)) => (cardsA, cardsB.appended(card))
      }
    val playerOneCards = Seq(cardsB.last).concat(cardsA)
    val playerTwoCards = cardsB.dropRight(1)
    assertEquals((playerOneCards.size, playerTwoCards.size), (27, 25))
    val (cardHands, _) = playGame(drawCards)
      .run(
        Map(PlayerOne -> playerOneCards, PlayerTwo -> playerTwoCards)
      )
      .value

    assertEquals(cardHands.get(PlayerOne).toSeq.flatten.size, 0)
    assertEquals(cardHands.get(PlayerTwo).toSeq.flatten.size, 52)

  }

}
