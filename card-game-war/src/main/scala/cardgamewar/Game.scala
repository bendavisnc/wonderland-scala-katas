package cardgamewar

import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cardgamewar.BattleResult.*
import cardgamewar.WarResult.*
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

  def drawCardsForWar: GameState[(List[Card], List[Card])] =
    def f(player: Player): GameState[List[Card]] =
      drawCard(player).replicateA(WarCardsCount).map(_.flatten)
    (f(PlayerOne), f(PlayerTwo)).tupled

  def battleWinner(player: Option[Player],
                   winningCard: Option[Card],
                   losingCard: Option[Card]
  ): GameState[BattleResult] =
    drawDiscardPile.flatMap { case cardsDiscarded =>
      Traverse[Seq]
        .traverse_(cardsDiscarded.toList ::: winningCard.toList ::: losingCard.toList)(
          (card: Card) => {
            State.modify[CardHands](
              (_.addCard(player.getOrElse(Discard), card)): CardHands => CardHands
            )
          }
        )
        .as((player, winningCard) match
          case (Some(p), Some(c)) => BattleWinner(player = p, card = c)
          case (None, _)          => War
          case _                  => ???)
    }

  def playBattle(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[BattleResult] =
    gamestate.flatMap {
      case (Some(cardA), Some(cardB)) if cardA > cardB =>
        battleWinner(player = Some(PlayerOne), winningCard = Some(cardA), losingCard = Some(cardB))
      case (Some(cardA), Some(cardB)) if cardA < cardB =>
        battleWinner(player = Some(PlayerTwo), winningCard = Some(cardB), losingCard = Some(cardA))
      case (Some(cardA), Some(cardB)) =>
        battleWinner(player = None, winningCard = Some(cardB), losingCard = Some(cardA))
      case (Some(cardA), None) =>
        battleWinner(player = Some(PlayerOne), winningCard = Some(cardA), losingCard = None)
      case (None, Some(cardB)) =>
        battleWinner(player = Some(PlayerTwo), winningCard = Some(cardB), losingCard = None)
      case _ => ???
    }

  def warWinner(player: Option[Player],
                winningCards: Cards,
                losingCards: Cards
  ): GameState[WarResult] =
    drawDiscardPile.flatMap { case cardsDiscarded =>
      Traverse[Seq]
        .traverse_(cardsDiscarded.toList ::: winningCards.toList ::: losingCards.toList)(
          (card: Card) => {
            State.modify[CardHands](
              (_.addCard(player.getOrElse(Discard), card)): CardHands => CardHands
            )
          }
        )
        .as((player, winningCards.lastOption) match {
          case (Some(p), Some(c)) => WarWinner(player = p, card = c)
          case (None, _)          => Draw
          case _                  => ???
        })
    }

  def playWar(
    gamestate: GameState[(List[Card], List[Card])]
  ): GameState[WarResult] =
    gamestate.flatMap {
      case (playerOneCards @ _ :: _ :: _ :: cardA :: Nil,
            playerTwoCards @ _ :: _ :: _ :: cardB :: Nil
          ) if cardA > cardB => {
        warWinner(player = Some(PlayerOne),
                  winningCards = playerOneCards,
                  losingCards = playerTwoCards
        )
      }

      case (playerOneCards @ _ :: _ :: _ :: cardA :: Nil,
            playerTwoCards @ _ :: _ :: _ :: cardB :: Nil
          ) if cardA < cardB => {
        warWinner(player = Some(PlayerTwo),
                  winningCards = playerTwoCards,
                  losingCards = playerOneCards
        )
      }

      case (playerOneCards @ _ :: _ :: _ :: _ :: Nil, playerTwoCards @ _ :: _ :: _ :: _ :: Nil) => {
        warWinner(player = None, winningCards = playerTwoCards, losingCards = playerOneCards)
      }

      case (playerOneCards @ _ :: _ :: _ :: _ :: Nil, playerTwoCards) => {
        warWinner(player = Some(PlayerOne),
                  winningCards = playerOneCards,
                  losingCards = playerTwoCards
        )
      }

      case (playerOneCards, playerTwoCards @ _ :: _ :: _ :: _ :: Nil) => {
        warWinner(player = Some(PlayerTwo),
                  winningCards = playerTwoCards,
                  losingCards = playerOneCards
        )
      }

      case (playerOneCards, playerTwoCards) => {
        warWinner(player = None, winningCards = playerOneCards, losingCards = playerTwoCards)
      }

    }

  def playRound(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[RoundResult] =
    playBattle(gamestate).flatMap {
      case battleWinner @ BattleWinner(_, _) => State.get.as(battleWinner)
      case War                               =>
        playWar(drawCardsForWar).map {
          case warWinner @ WarWinner(_, _) => warWinner
          case Draw                        => Draw
        }
    }

  def playGame(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[RoundResult] =
    State.apply(s => {
      Iterator
        .unfold(s)(s2 => {
          val (s3, a) = playRound(gamestate).run(s2).value
          Some(((s3, a), s3))
        })
        .take(1000)
        .find { case (cardHands, _) => cardHands.isGameOver }
        .getOrElse(throw Exception("Maximum game hands exceeded."))
    })

}
