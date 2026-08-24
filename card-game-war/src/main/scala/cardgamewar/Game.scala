package cardgamewar

import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cardgamewar.Outcome.*
import cats.syntax.all.*
import scala.annotation.tailrec
import cats.data.Kleisli
import cats.Applicative

object Game {

  val DiscardCount = 4

  def playTurnWinner(gameState: GameState, winner: Player): (GameState, Option[Card]) =
    val (cardHands, _)              = gameState
    val cardHandsUpdated: CardHands = Map(
      winner -> cardHands
        .get(winner)
        .toSeq
        .flatten
        .tail
        .appended(cardHands.get(winner).toSeq.flatten.head)
        .appended(cardHands.get(winner.other).toSeq.flatten.head)
        .concat(cardHands.get(Discard).toSeq.flatten),
      winner.other -> cardHands.get(winner.other).toSeq.flatten.tail,
      Discard      -> Seq.empty
    )
    val winnerOutcome: Option[Outcome] =
      if cardHandsUpdated.get(winner.other).toSeq.flatten.isEmpty then winner.wins.some else None
    val winningCard: Option[Card] = cardHands.get(winner).toSeq.flatten.headOption
    ((cardHandsUpdated, winnerOutcome), winningCard)

  def playTurn(gameState: GameState): (GameState, Option[Card]) =
    val (cardHands, outcome)                      = gameState
    val playerToFirstCard: Player => Option[Card] = cardHands.get(_).flatMap(_.headOption)
    (PlayerOne, PlayerTwo).bimap(playerToFirstCard, playerToFirstCard) match {
      // PlayerOne has the best card
      case (Some(cardA), Some(cardB)) if cardA > cardB => playTurnWinner(gameState, PlayerOne)

      // PlayerTwo has the best card
      case (Some(cardA), Some(cardB)) if cardA < cardB => playTurnWinner(gameState, PlayerTwo)

      // Neither player has enough cards to continue
      case _
          if cardHands
            .get(PlayerOne)
            .toSeq
            .flatten
            .size < DiscardCount && cardHands.get(PlayerTwo).toSeq.flatten.size < DiscardCount =>
        ((cardHands, Draw.some), None)

      // PlayerOne has enough cards to continue
      case _
          if cardHands
            .get(PlayerTwo)
            .toSeq
            .flatten
            .size < DiscardCount =>
        ((cardHands, PlayerOneWins.some), None)

      // PlayerTwo has enough cards to continue
      case _
          if cardHands
            .get(PlayerOne)
            .toSeq
            .flatten
            .size < DiscardCount =>
        ((cardHands, PlayerTwoWins.some), None)

      // Both players have enough cards to go into a tiebreak
      case (Some(cardA), Some(cardB)) =>
        ((Map(
            PlayerOne -> cardHands.get(PlayerOne).toSeq.flatten.tail.drop(DiscardCount - 1),
            PlayerTwo -> cardHands.get(PlayerTwo).toSeq.flatten.tail.drop(DiscardCount - 1),
            Discard   -> cardHands
              .get(Discard)
              .toSeq
              .flatten
              .appended(cardA)
              .concat(cardHands.get(PlayerOne).toSeq.flatten.tail.take(DiscardCount - 1))
              .appended(cardB)
              .concat(cardHands.get(PlayerTwo).toSeq.flatten.tail.take(DiscardCount - 1))
          ),
          None
         ),
         None
        )

      case _ => throw Exception(s"Unexpected card hands, `$cardHands`.")
    }

  def playRound(gameState: GameState): (GameState, Option[Card]) =
    val f: GameState => Option[((GameState, Option[Card]), GameState)] = gameState =>
      val (gameStateNext, cardOpt) = playTurn(gameState)
      ((gameStateNext, cardOpt), gameStateNext).some

    Iterator
      .unfold(gameState)(f)
      .collectFirst {
        case roundWinner @ (gameState, Some(winningCard)) => roundWinner
        case roundStoppage @ ((_, Some(_)), _)            => roundStoppage
      }
      .getOrElse(throw new Exception(s"Something bad happened during round play."))

  def playGame(gameState: GameState): (GameState, Outcome) =
    val f: GameState => Option[((GameState, Option[Card]), GameState)] = gameState =>
      val (gameStateNext, cardOpt) = playRound(gameState)
      ((gameStateNext, cardOpt), gameStateNext).some

    Iterator
      .unfold(gameState)(f)
      .collectFirst { case gameConclusion @ ((_, Some(_)), _) =>
        (gameConclusion._1, gameConclusion._1._2.get)
      }
      .getOrElse(throw new Exception(s"Something bad happened during game play."))

}
