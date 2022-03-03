package com.ing.loca
package example

import example.Account.*

import com.ing.loca.OperationType

type Money = BigDecimal

object Account {


}

object AccountEntity extends Entity {
  override type Operation = AccountOperation
  override type State = Money

  sealed trait AccountOperation extends OperationType
  case class Deposit(amount: Money) extends AccountOperation {
    override type ReturnValue = Boolean

    override def returnValue(state: Money): ReturnValue = true

    override def effect(state: Money): Money = state + amount
  }
  case class Withdraw(amount: Money) extends AccountOperation {
    override type ReturnValue = Boolean

    override def returnValue(state: Money): Boolean = amount <= state

    override def effect(state: Money): Money = if state - amount >= 0 then state - amount else state
  }

  //  //  TODO: return operation.RetVal?. Can work, but correct type inside match is not inferred (yet)
  //  override def returnValue[Op <: AccountOperation](state: Money, operation: Op): operation.ReturnValue =
  //    operation match {
  //      case d: Deposit  => true
  //      case w: Withdraw => if w.amount <= state then w.T else w.F
  //    }
  //
  //  override def effect(state: Money, operation: AccountOperation): Money = operation match {
  //    case d: Deposit  => state + d.amount
  //    case w: Withdraw => if state - w.amount >= 0 then state - w.amount else state
  //  }
}
