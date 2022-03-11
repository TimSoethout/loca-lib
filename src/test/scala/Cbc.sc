import com.ing.loca.example.AccountEntity
import com.ing.loca.example.AccountEntity.{Deposit, DepositWithBalance, Withdraw}
import com.ing.loca.{CbcDecider, example}

val decider = new CbcDecider(AccountEntity)

decider.allow(100, Seq(Deposit(10)), Deposit(10))

decider.allow(100, Seq(Withdraw(10), Withdraw(10), Withdraw(10)), Withdraw(10))
decider.allow(100, Seq(Withdraw(10), Deposit(10), Withdraw(10)), Withdraw(10))

decider.allow(100, Seq(DepositWithBalance(10)), DepositWithBalance(10))
