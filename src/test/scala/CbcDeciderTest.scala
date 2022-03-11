package com.ing.loca

import LoCADecider.{Accept, Delay, Reject}
import example.AccountEntity

import com.ing.loca.example.AccountEntity.{Deposit, Withdraw}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CbcDeciderTest extends AnyFlatSpec with should.Matchers {
  behavior of "CbcDecider"

  val decider: CbcDecider[AccountEntity.type] = new CbcDecider(AccountEntity)

  it should "allow parallel deposits" in {
    decider.allow(100, Seq(Deposit(10)), Deposit(10)) shouldBe Accept
  }

  it should "not allow parallel withdrawals insufficient balance is available" in {
    decider.allow(100, Seq(Withdraw(100)), Withdraw(100)) shouldBe Delay
  }

  it should "allow parallel withdrawals sufficient balance is available" in {
    decider.allow(100, Seq(Withdraw(10)), Withdraw(10)) shouldBe Accept
  }

  it should "allow parallel withdrawals sufficient balance is available with multiple in progress" in {
    decider.allow(100, Seq(Withdraw(10),Withdraw(10),Withdraw(10)), Withdraw(10)) shouldBe Accept
    decider.allow(100, Seq(Withdraw(10),Deposit(10),Withdraw(10)), Withdraw(10)) shouldBe Accept
  }

  //balance in return values to show where modeling influences condition
  "Example with balance in return values" should "not allow parallel processing" in {
    type Money = BigDecimal
    object AccountRetValEntity extends Entity {
      override type Operation = AccountOperation
      override type State = Money

      sealed trait AccountOperation extends OperationType
      case class Deposit(amount: Money) extends AccountOperation {
        override type ReturnValue = Money

        override def returnValue(state: Money): Money = state + amount

        override def effect(state: Money): Money = state + amount
      }
      case class Withdraw(amount: Money) extends AccountOperation {
        override type ReturnValue = Money

        override def returnValue(state: Money): Money = if state - amount >= 0 then state - amount else state

        override def effect(state: Money): Money = if state - amount >= 0 then state - amount else state
      }
    }
    val balRetValDecider: CbcDecider[AccountRetValEntity.type] = new CbcDecider(AccountRetValEntity)
    balRetValDecider.allow(100, Seq(AccountRetValEntity.Deposit(10)), AccountRetValEntity.Deposit(100)) shouldBe Delay
  }
}
