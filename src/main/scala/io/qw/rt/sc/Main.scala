package io.qw.rt.sc

object Main extends App {

  def entryPoint(seq: Seq[Transaction], blockchain: Blockchain): ConsoleModule[Any] = {

    (seq, blockchain) match {
      case (seq_, blockchain_) if seq_.size > 4 && blockchain_.iterate.isEmpty => {
        val blockchainNew = Miner.mineTransaction(seq_, Miner.Genesis)
        Miner.printlnBlockchain(blockchainNew)
        entryPoint(Seq.empty, blockchainNew)
      }
      case (seq_, blockchain_) if seq_.nonEmpty && seq_.size > 4 && blockchain_.iterate.nonEmpty => {
        var blockchainNew = Miner.mineTransaction(seq_, blockchain_)
        Miner.printlnBlockchain(blockchainNew)
        entryPoint(Seq.empty, blockchainNew)
      }
      case _ => {
        for {
          _ <- ConsoleModule.printLine("Enter transaction to add for a chain...")
          transaction <- ConsoleModule.readLine
          - <- ConsoleModule.printLine("Transaction added")
          _ <- entryPoint(seq :+ Transaction(transaction), blockchain)
        } yield ()
      }
    }
  }

  override def main(args: Array[String]): Unit = ConsoleModule.interpretator(entryPoint(Seq.empty, FastBlockchain(List.empty)))

}
