package io.qw.rt.sc

import java.security.MessageDigest

import org.apache.logging.log4j.core.Logger

import scala.concurrent.Future
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

object EntryLogic {

  type POW = Random

  type Bytes = Array[Byte]
  val Bytes = new Array[Byte](_: Int)

  type Number = BigInt
  val Number = BigInt

  def toHexString(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => String.format("%02X", Byte.box(b))).mkString("")
}

sealed trait SimpleBlockchain {
  def append(block: SimpleBlock): SimpleBlockchain

  def findByIndex(index: Int): Option[SimpleBlock]

  def findByHash(hash: Hash): Option[SimpleBlock]

  def parent(self: SimpleBlock): Option[SimpleBlock]

  def iterate: Seq[SimpleBlock]
}

case class Hash(bytes: Bytes) {
  def toNumber: Number = Number(1, bytes)

  def toHexString: String = EntryLogic.toHexString(bytes)
}

object Sha256 {

  val NumberOfBytes = 32
  val TheDigest = MessageDigest.getInstance("SHA-256")

  val Zero_Hash: Hash = Hash.apply(Array.fill[Byte](32)(0))

  def apply(bytess: Bytes*): Hash = {
    for (bytes <- bytess) {
      TheDigest.update(bytes)
    }
    val hash = TheDigest.digest()
    assert(hash.length == NumberOfBytes)

    Hash(hash)
  }
}


case class Transaction(data: String)

case class SimpleBlock(
  index: Int,
  parentHash: Hash,
  selfHash: Hash,
  transactions: Seq[Transaction],
  miningTargetNumber: Number,
  pow: Long
) {

  def cryptoHash: Hash = Sha256.apply(transactions.foldLeft("")(_ + _.data).getBytes)

  def verifyThisHasBeenMinedProperly(): Unit =
    assert(cryptoHash.toNumber < miningTargetNumber)

  override def toString: String = {
    s"The index: ${index}" + "\n" + s"The self hash: ${selfHash.toHexString}" + "\n" + s"The parent hash: ${parentHash.toHexString}" + "\n" +
    s"The mining target number: ${miningTargetNumber}" + "\n" + s"The PoW: ${pow}" + "\n" + s"The transactions: ${transactions}" + "\n"
  }
}

object SimpleMiner {

  final val StdMiningTargetNumber = targetByLeadingZeros(1)

  final val Genesis: SimpleBlockchain = {
    val block = SimpleBlock(0, Sha256.Zero_Hash, Sha256.Zero_Hash, Seq(Transaction("Start SimpleBlockchain, we got money :)")), StdMiningTargetNumber, 0)
    FastBlockchain(List.empty).append(block)
  }

  def targetByLeadingZeros(zeros: Int): BigInt = {
    require(zeros < Sha256.NumberOfBytes)

    val bytes: Bytes =
      Array.tabulate[Byte](32) { n =>
        if (n < zeros) {
          0
        }
        else {
          0xff.toByte
        }
      }

    BigInt(1, bytes)
  }

  def mineNextBlock(
    index: Int,
    parentHash: Hash,
    transactions: Seq[Transaction],
    miningTargetNumber: BigInt,
    blockchain: SimpleBlockchain
  ): SimpleBlockchain = {
    val hash = Hash.apply(transactions.foldLeft("")(_ + _.data).getBytes)
    var random: Int = Random.nextInt(1000000)
    val num = BigInt(hash.toNumber.intValue())
    while((num + random) < miningTargetNumber.intValue()){
      random = Random.nextInt(100)
    }
    val pow = random
    val block = SimpleBlock(index, parentHash, hash, transactions, miningTargetNumber, pow)
    blockchain.append(block)
  }

  def findPow(num: Number, random: Int): Future[Int] = {
    var randomNew = random
    Future { findPowRes(num, random) }
  }

  def findPowRes(num: EntryLogic.Number, random: Int): Int = {
    while ((num + random) < StdMiningTargetNumber.intValue()) {
      findPowRes(num, Random.nextInt(1000000))
    }
    random
  }

  def mineTransaction(transactions: Seq[Transaction], blockchain: SimpleBlockchain): SimpleBlockchain = {
    val selfHash = Hash(transactions.foldLeft("")(_ + _.data).getBytes)
    var random: Int = Random.nextInt(1000000)
    val num = BigInt(selfHash.toNumber.intValue())
    val size = blockchain.iterate.size
    val pow: Int = extractResult(findPow(num, random).value, num, random)
    val parentHash = blockchain.findByIndex(size-1) match {
      case None => Sha256.Zero_Hash
      case Some(value) => value.selfHash
    }
    val block = SimpleBlock(size, parentHash, selfHash, transactions, StdMiningTargetNumber, pow)
    blockchain.append(block)
  }

  def extractResult(maybeTriedInt: Option[Try[Int]], num: BigInt, random: Int): Int = maybeTriedInt match {
    case Some(value) => value match {
      case Failure(exception) => extractResult(findPow(num, random).value, num, random)
      case Success(value) => value
    }
    case None => extractResult(findPow(num, random).value, num, random)
  }


  def printlnBlockchain(blockchain: SimpleBlockchain) = {
    println("\n")
    println("SimpleBlockchain updated...")
    println("\n")
    blockchain.iterate.foreach(block => {
      println(block)
    })
    println("\n")
  }
}

case class FastBlockchain(blocks: List[SimpleBlock]) extends SimpleBlockchain {
  override def append(block: SimpleBlock): SimpleBlockchain = FastBlockchain(block :: blocks)

  override def findByIndex(index: Int): Option[SimpleBlock] = blocks.find(block => block.index == index)

  override def findByHash(hash: Hash): Option[SimpleBlock] = blocks.find(block => block.selfHash == hash)

  override def parent(self: SimpleBlock): Option[SimpleBlock] = blocks.find(block => block.parentHash == self.parentHash)

  override def iterate: Seq[SimpleBlock] = blocks

}

