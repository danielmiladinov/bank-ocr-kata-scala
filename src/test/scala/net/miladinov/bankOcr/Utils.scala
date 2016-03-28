package net.miladinov.bankOcr

import scala.io.Source

object Utils {
  def testFile (fileName: String) = Source.fromInputStream(getClass.getResourceAsStream(fileName))
  def formatGlyphStrings (lines: String*) = lines.map(formatGlyphString).toVector
  def formatGlyphString (repr: String) = repr.stripMargin.split("\n").toVector
}
