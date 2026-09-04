package cardgamewar

import cardgamewar.AceCard.*
import cardgamewar.DiscardPile.*
import cardgamewar.FaceCard.*
import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.BattleResult.*
import cardgamewar.Suit.*

class GameTomAndMaryTest extends munit.FunSuite {

  test("play game, turn one") {
    val tomsCards            = Seq(Card(4, Club))
    val marysCards           = Seq(Card(8, Spade))
    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)
    val (cardHandsNext, BattleWinner(winningPlayer, winningCard)) =
      playGame(drawCards).run(cardHands).value: @unchecked
    assertEquals(
      winningPlayer,
      PlayerTwo
    )
    assertEquals(
      winningCard,
      marysCards.head
    )
    assertEquals(cardHandsNext(PlayerOne).size, 0)
    assertEquals(cardHandsNext(PlayerTwo).size, 2)
    assertEquals(cardHandsNext.get(Discard).toSeq.flatten.size, 0)
  }

  test("play game, turn two") {
    val tomsCards  = Seq(Card(Ace, Spade))
    val marysCards = Seq(Card(King, Spade))

    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)
    val (cardHandsNext, BattleWinner(winningPlayer, winningCard)) =
      playGame(drawCards).run(cardHands).value: @unchecked
    assertEquals(
      winningPlayer,
      PlayerOne
    )
    assertEquals(
      winningCard,
      tomsCards.head
    )
    assertEquals(cardHandsNext(PlayerOne).size, 2)
    assertEquals(cardHandsNext(PlayerTwo).size, 0)
    assertEquals(cardHandsNext.get(Discard).toSeq.flatten.size, 0)

  }

  test("play game, turn three") {
    val tomsCards =
      Seq(Card(6, Spade), Card(2, Spade), Card(3, Spade), Card(4, Spade), Card(8, Diamond))
    val marysCards =
      Seq(Card(6, Diamond), Card(2, Diamond), Card(3, Diamond), Card(4, Diamond), Card(9, Heart))

    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)

    val (cardHandsNext, _) =
      playGame(drawCards).run(cardHands).value: @unchecked

    assertEquals(cardHandsNext(PlayerOne).size, 0)
    assertEquals(cardHandsNext(PlayerTwo).size, tomsCards.size + marysCards.size)
    assertEquals(cardHandsNext.get(Discard).toSeq.flatten.size, 0)

  }

}
