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

  private def compareCards(playerOneCards: Cards, playerTwoCards: Cards): Option[Player] =
    (playerOneCards.lastOption, playerTwoCards.lastOption) match
      case (Some(cardA), Some(cardB)) if cardA > cardB => Some(PlayerOne)
      case (Some(cardA), Some(cardB)) if cardA < cardB => Some(PlayerTwo)
      case (Some(_), Some(_))                          => None
      case (Some(_), None)                             => Some(PlayerOne)
      case (None, Some(_))                             => Some(PlayerTwo)
      case _                                           => None

  private def awardCards[A](
    player: Option[Player],
    winningCards: Cards,
    losingCards: Cards,
    tieResult: => A,
    winner: (Player, Card) => A
  ): GameState[A] =
    drawDiscardPile.flatMap { case cardsDiscarded =>
      Traverse[Seq]
        .traverse_(cardsDiscarded.toList ::: winningCards.toList ::: losingCards.toList)(
          (card: Card) => {
            State.modify[CardHands](
              (_.addCard(player.getOrElse(Discard), card)): CardHands => CardHands
            )
          }
        )
        .as((player, winningCards.lastOption) match
          case (Some(p), Some(c)) => winner(p, c)
          case (None, _)          => tieResult
          case _                  => ???)
    }

  def battleWinner(player: Option[Player],
                   winningCard: Option[Card],
                   losingCard: Option[Card]
  ): GameState[BattleResult] =
    awardCards(player = player,
               winningCards = winningCard.toSeq,
               losingCards = losingCard.toSeq,
               tieResult = War,
               winner = BattleWinner.apply
    )

  def playBattle(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[BattleResult] =
    gamestate.flatMap { case (cardA, cardB) =>
      compareCards(cardA.toSeq, cardB.toSeq) match
        case Some(PlayerOne) =>
          battleWinner(player = Some(PlayerOne), winningCard = cardA, losingCard = cardB)
        case Some(PlayerTwo) =>
          battleWinner(player = Some(PlayerTwo), winningCard = cardB, losingCard = cardA)
        case None => battleWinner(player = None, winningCard = cardB, losingCard = cardA)
    }

  def warWinner(player: Option[Player],
                winningCards: Cards,
                losingCards: Cards
  ): GameState[WarResult] =
    awardCards(player = player,
               winningCards = winningCards,
               losingCards = losingCards,
               tieResult = Draw,
               winner = WarWinner.apply
    )

  def playWar(
    gamestate: GameState[(List[Card], List[Card])]
  ): GameState[WarResult] =
    gamestate.flatMap { case (playerOneCards, playerTwoCards) =>
      compareCards(playerOneCards, playerTwoCards) match
        case Some(PlayerOne) =>
          warWinner(player = Some(PlayerOne),
                    winningCards = playerOneCards,
                    losingCards = playerTwoCards
          )
        case Some(PlayerTwo) =>
          warWinner(player = Some(PlayerTwo),
                    winningCards = playerTwoCards,
                    losingCards = playerOneCards
          )
        case None =>
          warWinner(player = None, winningCards = playerTwoCards, losingCards = playerOneCards)
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
