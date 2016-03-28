package net.miladinov.bankOcr

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks
import net.miladinov.bankOcr.Utils._

class ParserTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  "Parser" should "parse an account number from its textual representation" in {
    val representation = formatGlyphString {
      """    _  _     _  _  _  _  _ 
        |  | _| _||_||_ |_   ||_||_|
        |  ||_  _|  | _||_|  ||_| _|"""
    }

    Parser.parse(representation) shouldEqual "123456789"
  }
  
  val reprModelsAndExpectedResults = Table(
    ("representation", "expectedResult"),
    (""" _  _  _  _  _  _  _  _  _ 
       || || || || || || || || || |
       ||_||_||_||_||_||_||_||_||_|""",
      "000000000"
    ),
    ("""                           
       |  |  |  |  |  |  |  |  |  |
       |  |  |  |  |  |  |  |  |  |""",
      "111111111"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ """,
      "222222222"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       | _| _| _| _| _| _| _| _| _|""",
      "333333333"
    ),
    ("""                           
       ||_||_||_||_||_||_||_||_||_|
       |  |  |  |  |  |  |  |  |  |""",
      "444444444"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       | _| _| _| _| _| _| _| _| _|""",
      "555555555"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       ||_||_||_||_||_||_||_||_||_|""",
      "666666666"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       |  |  |  |  |  |  |  |  |  |
       |  |  |  |  |  |  |  |  |  |""",
      "777777777"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       ||_||_||_||_||_||_||_||_||_|""",
      "888888888"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       | _| _| _| _| _| _| _| _| _|""",
      "999999999"
    )
  )
  
  forAll (reprModelsAndExpectedResults) { (reprModel: String, expectedResult: String) => {
    it should s"be able to parse the representation of $expectedResult" in {
      val actualResult = Parser.parse(formatGlyphString(reprModel))
      actualResult shouldEqual expectedResult
    }
  }}
}
