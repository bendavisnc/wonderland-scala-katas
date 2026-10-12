package fox_goose_bag_of_corn

object Steps {

  def fromAndTo(youPosition: YouPosition): (RiverPosition, RiverPosition) =
    youPosition match {
      case YouPosition.BoatFromLeft  => (RiverPosition.Boat, RiverPosition.RightBank)
      case YouPosition.BoatFromRight => (RiverPosition.Boat, RiverPosition.LeftBank)
      case YouPosition.LeftBank      => (RiverPosition.LeftBank, RiverPosition.Boat)
      case YouPosition.RightBank     => (RiverPosition.RightBank, RiverPosition.Boat)
    }

  def everyPossibleNextSteps(mostRecent: RiverCrossingStep,
                             from: RiverPosition,
                             to: RiverPosition
  ): Seq[RiverCrossingStep] =
    // for everything that you can carry where you currently are...
    val itemsToCarry: Set[Option[RiverCrossingEntity]] =
      mostRecent(from.ordinal).diff(Set(🤷, ⛵)).map(Option.apply).incl(None)
    // ... include a step possibility that includes you and that item at that `to` position
    itemsToCarry.toSeq
      .map {
        case None => // ... but also account for the possibility of you carrying nothing at all
          mostRecent
            .updated(from.ordinal, mostRecent(from.ordinal).diff(Set(🤷)))
            .updated(to.ordinal, mostRecent(to.ordinal).union(Set(🤷)))
        case Some(carryItem) =>
          mostRecent
            .updated(from.ordinal, mostRecent(from.ordinal).diff(Set(🤷, carryItem)))
            .updated(to.ordinal, mostRecent(to.ordinal).union(Set(🤷, carryItem)))
      }
      // filter out any generated steps that contradict the rules
      .filter(Logic.isBoatCapacityRespected)
      .filter(Logic.isEveryoneSafe)

  def everyPossibleNextSteps(steps: Seq[RiverCrossingStep]): Seq[RiverCrossingStep] =
    val comingFrom: YouPosition = Logic.findWhereAreYou(steps)
    val mostRecent              = steps.last
    val (from, to)              = fromAndTo(comingFrom)
    everyPossibleNextSteps(mostRecent, from, to)

}
