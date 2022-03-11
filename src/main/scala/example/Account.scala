package com.ing.loca
package example

import com.ing.loca.OperationType

type Money = BigDecimal

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

  case class DepositWithBalance(amount: Money) extends AccountOperation {
    override type ReturnValue = Money

    override def returnValue(state: Money): ReturnValue = state + amount

    override def effect(state: Money): Money = state + amount
  }
}
