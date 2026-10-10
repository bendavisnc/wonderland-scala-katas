package fox_goose_bag_of_corn

import RiverCrossingEntity.*
import scala.collection.immutable.Queue

object Puzzle {

  val startPosition: Seq[RiverCrossingStep] =
    List(List(Set(Fox, Goose, Corn, You), Set(Boat), Set()))

  def riverCrossingPlan(): Seq[RiverCrossingStep] =
    def loop(q: Queue[Seq[RiverCrossingStep]]): Seq[RiverCrossingStep] =
      val (qFirst, qRest) = q.dequeue
      if Logic.everyoneMadeIt(qFirst.last) then qFirst
      else loop(qRest.enqueueAll(Steps.everyPossibleNextSteps(qFirst).map(qFirst.appended)))
    loop(Queue(startPosition))

}
