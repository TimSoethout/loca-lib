package com.ing.loca

import LoCADecider.*

object LoCADecider {
  sealed trait AllowDecision
  //  TODO maybe add reject and delay reasons
  object Accept extends AllowDecision
  object Reject extends AllowDecision
  object Delay extends AllowDecision
}

class CbcDecider[E <: Entity](val entity: E) {

  import entity._

  /**
   * Implement CBC* (ccbc) From CBC paper.
   */
  //  DynamicCBC(s, p, q) == /\ RetVal(p, s) = RetVal(p, Eff(q, s))
  //                         /\ RetVal(q, Eff(p, s)) = RetVal(q, s)
  def cbc(state: State, operation: Operation, incomingOperation: Operation): Boolean = {
    operation.returnValue(state) == operation.returnValue(incomingOperation.effect(state)) &&
      incomingOperation.returnValue(operation.effect(state)) == incomingOperation.returnValue(state)
//    returnValue(state, operation) == returnValue(effect(state, incomingOperation), operation) &&
//      returnValue(effect(state, operation), incomingOperation) == returnValue(state, incomingOperation)
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
      case ((prevState, result), op) => (op.effect(prevState), result && cbc(prevState, op, incomingOperation))
    }._2
  }

  def allow(state: entity.State, inProgressOperations: Seq[entity.Operation], incomingOperation: entity.Operation): AllowDecision = {
    if (ccbc(state, inProgressOperations, incomingOperation)) Accept else Delay
  }
}