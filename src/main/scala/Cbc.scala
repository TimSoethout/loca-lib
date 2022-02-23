package com.ing.loca

import LoCADecider.*

object LoCADecider {
  sealed trait AllowDecision
  //  TODO maybe add reject and delay reasons
  object Accept extends AllowDecision
  object Reject extends AllowDecision
  object Delay extends AllowDecision

  val MaxInProgressReachedDelay: AllowDecision = Delay //(NonEmptyChain(MaxInProgressReachedDelayReason), Set.empty)
}

/**
 *
 */
class CbcDecider[E <: Entity](val entity: E) {

  import entity._

  /**
   * Implement CBC* (ccbc) From CBC paper.
   * cbc*(𝑠,𝑂,𝑜𝑖)=∀𝑜𝑛 ∈𝑂.cbc(𝑠1..𝑛−1,𝑜𝑛,𝑜𝑖)
   */
  //  DynamicCBC(s, p, q) == /\ RetVal(p, s) = RetVal(p, Eff(q, s))
  //                         /\ RetVal(q, Eff(p, s)) = RetVal(q, s)
  def cbc(state: State, operation: Operation, incomingOperation: Operation): Boolean = {
    returnValue(state, operation) == returnValue(effect(state, incomingOperation), operation) &&
      returnValue(effect(state, operation), incomingOperation) == returnValue(state, incomingOperation)
  }

  //  ConstructiveCBC(state, inProgressOps, incomingOp) ==
  //  LET accummulator == [ result |-> TRUE, nextState |-> state ]
  //  IN ReduceSeq(LAMBDA o, acc:
  //  \* [prevState EXCEPT ![o.key] = Eff(o.op, prevState[o.key])]
  //    [ result |-> acc.result /\ CBC(acc.nextState, o, incomingOp)
  //    , nextState |-> Eff(o, acc.nextState)
  //    ]
  //  , inProgressOps, accummulator).result
  //  * cbc*(𝑠,𝑂,𝑜𝑖)=∀𝑜𝑛 ∈𝑂.cbc(𝑠1..𝑛−1,𝑜𝑛,𝑜𝑖)
  def ccbc(state: State, inProgressOperations: Seq[Operation], incomingOperation: Operation): Boolean = {
    // fold over in progress operations to compute the intermediate states
    // for each intermediate state determine if incoming operation is CBC
    inProgressOperations.foldLeft((state, true)) {
      case ((prevState, result), op) => (entity.effect(prevState, op), result && cbc(prevState, op, incomingOperation))
    }._2
  }

  def allow(state: entity.State, inProgressOperations: Seq[entity.Operation], incomingOperation: entity.Operation): AllowDecision = {
    if (ccbc(state, inProgressOperations, incomingOperation)) Accept else Delay
  }
}

//object IEDecider {
//  //  Do not allow further parallelism if the tree depth is reached
//  val maxTransactionsInProgress: Int = 7
//
//  def allow(entity: Entity)
//           (state: entity.State, inProgressOperations: Seq[entity.Operation], incomingOperations: entity.Operation)
//  : AllowDecision =
//    if (inProgressOperations.size >= maxTransactionsInProgress) {
//      // Delay when configured maximum parallelism is reached
//      MaxInProgressReachedDelay
//    } else {
//      // Algorithm:
//      // Iterate over all possible state/data outcomes. Try apply command & make sure sync operations do not changes by effects of in progress actions
//      // All OK => Accept / OK
//      // All NOK => Reject / NOK
//      // otherwise => Delay / stash
//      // Future work: reorder
//
//      type PossibleOutcomes = Seq[entity.State]
////      type EventEffector = (entity.State, entity.Operation) => entity.State
//
//      val initialPossibleOutcomes: PossibleOutcomes = Seq(state)
//
////      val eventEffector: EventEffector =
////        (state, operation) => {
////          val newState =
////            entity.effect(state, operation)
////          newState.getOrElse {
////            throw new NotImplementedError(
////              s"Should not occur for $operation $newState in currentState/Data/syncOps: ${state}/${state}")
////          }
////        }
//
//      /**
//       * Calculates new possible outcomes, given current possible outcomes. Since the Possible Outcome Tree branches
//       * into one where the event is applied and one where not, we can keep the old list of outcome states and add a version
//       * with states where the event is applied
//       */
//      def newPossibleOutcomes(outcomes: PossibleOutcomes, operation: entity.Operation): PossibleOutcomes = {
//        outcomes.concat(outcomes.map(state => entity.effect(state, operation)))
//      }
//
//      // compute all possible outcomes for all in progress event
//      val possibleOutcomes: PossibleOutcomes = inProgressOperations.foldLeft(initialPossibleOutcomes) {
//        // TODO we can cache newPossibleOutcomes, because start of list will be the same if no actions completed in the mean time since previous check
//        case (outcomes, event) => newPossibleOutcomes(outcomes, event)
//      }
//
//      // Compute set of sync operations for new event in current state
//      // Empty Set is safe because isActionAllowedInState would fail anyway
////      val syncOperationsIncomingEvent: Set[SyncOperation] = specification.syncOperationsGeneric(currentData, incomingEvent).getOrElse(Set())
//      // For all possible action check if incoming event results in Accept/Reject/Delay
////      val newActionInTentativeStates: NonEmptyChain[AllowCommandDecision] =
////        possibleOutcomes.map(isActionAllowedInState(_, incomingEvent, syncOperationsIncomingEvent))
////
//      val allOk: Boolean = possibleOutcomes.forall(_.isInstanceOf[Accept.type])
//      if (allOk) {
//        // direct accept
//        Accept
//      } else {
//        val allReject: Boolean = possibleOutcomes.forall(_.isInstanceOf[Reject])
//        // uses SemiGroup defined above, not that allReject case is covered by SemiGroup as well. Kept for code readability
//        val rejectOrDelay: AllowCommandDecision = newActionInTentativeStates.reduce
//        if (allReject) {
//          // direct reject
//          assert(rejectOrDelay.isInstanceOf[Reject], s"If not all Accept and all Reject, there should be at least one Reject/error message: $rejectOrDelay")
//          rejectOrDelay
//        } else {
//          // delay
//          assert(rejectOrDelay.isInstanceOf[Delay], s"If not all Accept and not all Reject, there should be at least one Delay message: $rejectOrDelay")
//          rejectOrDelay
//        }
//      }
//    }
//}