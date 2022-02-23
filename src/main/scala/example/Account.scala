package com.ing.loca
package example

import example.Account._

type Money = BigDecimal

object Account {
  sealed trait AccountOperation
  case class Deposit(amount: Money) extends AccountOperation
  case class Withdraw(amount: Money) extends AccountOperation
}

object AccountEntity extends Entity {
  override type Operation = AccountOperation
  override type State = Money

//  TODO: return operation.RetVal?. Can work, but correct type inside match is not inferred (yet)
  override def returnValue(state: Money, operation: AccountOperation): Any = operation match {
    case _: Deposit  => true
    case w: Withdraw => w.amount <= state
  }

  override def effect(state: Money, operation: AccountOperation): Money = operation match {
    case d: Deposit  => state + d.amount
    case w: Withdraw => if state - w.amount >= 0 then state - w.amount else state
  }
}
