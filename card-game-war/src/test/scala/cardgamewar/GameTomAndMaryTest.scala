package cardgamewar

import cardgamewar.AceCard.*
import cardgamewar.DiscardPile.*
import cardgamewar.FaceCard.*
import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.BattleResult.*
import cardgamewar.Suit.*
import cardgamewar.tuple2IntCard
import cardgamewar.tuple2RankCard

class GameTomAndMaryTest extends munit.FunSuite {

  test("play game, turn one") {
    val tomsCards            = Seq[Card]((4, ♣))
    val marysCards           = Seq[Card]((8, ♠))
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
    val tomsCards  = Seq[Card]((Ace, ♠))
    val marysCards = Seq[Card]((King, ♠))

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
      Seq[Card]((6, ♠), (2, ♠), (3, ♠), (4, ♠), (8, ♦))
    val marysCards =
      Seq[Card]((6, ♦), (2, ♦), (3, ♦), (4, ♦), (9, ♥))

    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)

    val (cardHandsNext, _) =
      playGame(drawCards).run(cardHands).value: @unchecked

    assertEquals(cardHandsNext(PlayerOne).size, 0)
    assertEquals(cardHandsNext(PlayerTwo).size, tomsCards.size + marysCards.size)
    assertEquals(cardHandsNext.get(Discard).toSeq.flatten.size, 0)

  }

}
