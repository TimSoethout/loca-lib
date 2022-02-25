package com.ing.loca

import LoCADecider.{Accept, Delay, Reject}
import example.Account.{Deposit, Withdraw}
import example.AccountEntity

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
}
