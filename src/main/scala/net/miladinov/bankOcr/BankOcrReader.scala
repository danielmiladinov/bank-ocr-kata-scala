package net.miladinov.bankOcr

import scala.io.Source

class BankOcrReader {
  def read (source: Source): IndexedSeq[IndexedSeq[String]] = 
    source.getLines()
      .toIndexedSeq
      .grouped(4)
      .map(_.init)
      .toIndexedSeq
}
