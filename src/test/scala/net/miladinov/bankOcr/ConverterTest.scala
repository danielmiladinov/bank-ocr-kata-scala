package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import net.miladinov.bankOcr.Utils._

class ConverterTest extends FlatSpec with ShouldMatchers {
  "Converter" should "read an input file containing many account number glyphs and produce a string containing their digits" in {
    val fiveHundredAccountGlyphs = testFile("/500-accounts-glyphs.txt")
    val expectedOutput = testFile("/500-accounts-numbers.txt").getLines().mkString("\n")
    
    Converter.convert(fiveHundredAccountGlyphs).mkString("\n") shouldEqual expectedOutput
  }
}
