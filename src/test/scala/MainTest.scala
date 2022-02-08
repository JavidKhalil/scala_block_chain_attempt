import io.qw.rt.sc.ConsoleModule.succeed
import io.qw.rt.sc._
import org.scalatest._
import flatspec._
import matchers._
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.verbs.ShouldVerb
import scala.util.Random

/**
* EntryLogic and only test class
* we use scalatest for testing functionality
*/
class MainTest extends AnyFlatSpec with should.Matchers {

  val blockchain = Miner.Genesis
  val transactions = Seq(Transaction("transaction one"), Transaction("transaction two"))

  "SimpleMiner Genesis " should " generate a blockchain " in {
    Miner.Genesis shouldBe a [Blockchain]
  }

  "SimpleMiner mineTransaction " should " add new transaction to a blockchain " in {
    assert(Miner.mineTransaction(transactions, blockchain).iterate.size == 2)
  }

  "SimpleMiner findPow " should " generate a pow which lower than target number " in {
    assert(Miner.findPowRes(BigInt(1), Random.nextInt(10)) > Miner.StdMiningTargetNumber.intValue())
  }

}
