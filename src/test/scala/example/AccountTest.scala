package com.ing.loca
package example

import com.ing.loca.example.Account.{Deposit, Withdraw}
import org.scalatest.*
import flatspec.*
import matchers.*

class AccountTest extends AnyFlatSpec with should.Matchers {

  "An Account" should "deposit" in {
    val balance = 100
    val newBalance = AccountEntity.effect(balance, Deposit(10))
    newBalance shouldBe 110
  }

  it should "withdraw" in {
    AccountEntity.effect(100, Withdraw(10)) shouldBe 90
  }

  it should "not withdraw with insufficient balance" in {
    AccountEntity.effect(100, Withdraw(110)) shouldBe 100
  }

  it should "always deposit" in {
    AccountEntity.returnValue(100, Deposit(10)) shouldBe true
  }

  it should "withdraw on enough balance only" in {
    AccountEntity.returnValue(100, Withdraw(10)) shouldBe true
    AccountEntity.returnValue(100, Withdraw(110)) shouldBe false
  }

//  behavior of "LoCA"
//
//  it should "allow parallel deposits" in {
//    LoCA.allow(100, Seq(Deposit(10)), Deposit(10)) shouldBe true
//  }
//
//  it should "not allow parallel withdrawals insufficient balance is available" in {
//    LoCA.allow(100, Seq(Withdraw(100)), Withdraw(100)) shouldBe false
//  }
//
//  it should "allow parallel withdrawals sufficient balance is available" in {
//    LoCA.allow(100, Seq(Withdraw(100)), Withdraw(100)) shouldBe true
//  }
}
