package net.miladinov.bankOcr

import scala.io.Source

object Converter {

  def convert (input: Source): IndexedSeq[String] = {
    val glyphs = Reader.read(input)
    glyphs.map(Parser.parse)
  }
}
