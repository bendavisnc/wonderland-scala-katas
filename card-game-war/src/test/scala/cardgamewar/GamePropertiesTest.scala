package cardgamewar

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import cardgamewar.Game.*
import cardgamewar.DiscardPile.*
import cardgamewar.Player.*
import cardgamewar.WarResult.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Helpers.h
import cardgamewar.BattleResult.BattleWinner

class GamePropertiesTest extends ScalaCheckSuite {

  property("game always ends with no cards missing") = forAll { (cardHands: CardHands) =>
    playGame(drawCards).run(cardHands).value._1.values.map(_.size).sum == 52
  }

  property("game always ends with one player with no cards.") = forAll { (cardHands: CardHands) =>
    val result        = playGame(drawCards).run(cardHands).value
    val loser: Player = result._1.-(Discard).find(_._2.isEmpty).get._1.asInstanceOf[Player]
    result._1.apply(loser).size == 0 &&
    (result._2 match
      case BattleWinner(winner, _) => winner != loser
      case WarWinner(winner, _)    => winner != loser
      case Draw                    => true)

  }

}
