package cardgamewar

import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cats.syntax.all.*
import scala.annotation.tailrec
import cats.data.State
import cats.Applicative
import cats.Traverse
import pprint.pprintln

object Game {

  def drawCard(player: Player): GameState[Option[Card]] = State(_.drawCard(player))

  def drawDiscardPile: GameState[Seq[Card]] = State(_.drawDiscardPile)

  def drawCards: GameState[(Option[Card], Option[Card])] =
    (drawCard(PlayerOne), drawCard(PlayerTwo)).tupled

  def drawCardsForWar: GameState[(Option[List[Card]], Option[List[Card]])] =
    def f(player: Player) = Traverse[Seq]
      .traverse(Seq.fill(WarCardsCount)(player))(drawCard)
      .map(_.sequence)
      .map(_.map(_.toList))
    (f(PlayerOne), f(PlayerTwo)).tupled

  def battleWinner(player: Option[Player],
                   winningCard: Card,
                   losingCard: Card
  ): GameState[Option[BattleWinner]] =
    drawDiscardPile.flatMap { case cardsDiscarded =>
      Traverse[Seq]
        .traverse(winningCard :: losingCard :: cardsDiscarded.toList)((card: Card) => {
          State.modify[CardHands](
            (_.addCard(player.getOrElse(Discard), card)): CardHands => CardHands
          )
        })
        .as(player.map(p => BattleWinner(player = p, card = winningCard)))
    }

  def playBattle(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[BattleWinner]] =
    gamestate.flatMap {
      case (Some(cardA), Some(cardB)) if cardA > cardB =>
        battleWinner(player = Some(PlayerOne), winningCard = cardA, losingCard = cardB)
      case (Some(cardA), Some(cardB)) if cardA < cardB =>
        battleWinner(player = Some(PlayerTwo), winningCard = cardB, losingCard = cardA)
      case (Some(cardA), Some(cardB)) =>
        battleWinner(player = None, winningCard = cardB, losingCard = cardA)
      case _ => throw Exception("One or both players are missing a card.")
    }

  def warWinner(player: Option[Player],
                winningCards: Cards,
                losingCards: Cards
  ): GameState[Option[WarWinner]] =
    drawDiscardPile.flatMap { case cardsDiscarded =>
      val acc =
        if player == Some(PlayerOne) then winningCards.concat(losingCards).concat(cardsDiscarded)
        else losingCards.concat(winningCards).concat(cardsDiscarded)
      Traverse[Seq]
        .traverse(acc)((card: Card) => {
          State.modify[CardHands](
            (_.addCard(player.getOrElse(Discard), card)): CardHands => CardHands
          )
        })
        .as(player.map(p => WarWinner(player = p, card = winningCards.last)))
    }

  def playWar(
    gamestate: GameState[(Option[List[Card]], Option[List[Card]])]
  ): GameState[Option[WarWinner]] =
    gamestate.flatMap {
      case (Some(playerOneCards @ _ :: _ :: _ :: cardA :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: cardB :: Nil)
          ) if cardA > cardB => {
        warWinner(player = Some(PlayerOne),
                  winningCards = playerOneCards,
                  losingCards = playerTwoCards
        )
      }

      case (Some(playerOneCards @ _ :: _ :: _ :: cardA :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: cardB :: Nil)
          ) if cardA < cardB => {
        warWinner(player = Some(PlayerTwo),
                  winningCards = playerTwoCards,
                  losingCards = playerOneCards
        )
      }

      case (Some(playerOneCards @ _ :: _ :: _ :: _ :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: _ :: Nil)
          ) => {
        warWinner(player = None, winningCards = playerTwoCards, losingCards = playerOneCards)
      }

      case (None, None) =>
        State.get.as(None)

      case _ => throw Exception("One player doesn't have enough cards for war.")

    }

  def playRound(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[RoundWinner]] =
    playBattle(gamestate).flatMap {
      case battleWinner @ Some(_) => State.get.as(battleWinner)
      case _                      => playWar(drawCardsForWar).map(a => a: Option[RoundWinner])
    }

  def playGame(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[RoundWinner]] =
    State.apply(s => {
      Iterator
        .unfold(s)(s2 => {
          val (s3, a) = playRound(gamestate).run(s2).value
          Some(((s3, a), s3))
        })
        .find { case (cardHands, _) => cardHands.isGameOver }
        .get
    })

}
