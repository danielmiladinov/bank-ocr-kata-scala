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
      "111111111 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ """,
      "222222222 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       | _| _| _| _| _| _| _| _| _|""",
      "333333333 ERR"
    ),
    ("""                           
       ||_||_||_||_||_||_||_||_||_|
       |  |  |  |  |  |  |  |  |  |""",
      "444444444 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       | _| _| _| _| _| _| _| _| _|""",
      "555555555 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       ||_||_||_||_||_||_||_||_||_|""",
      "666666666 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       |  |  |  |  |  |  |  |  |  |
       |  |  |  |  |  |  |  |  |  |""",
      "777777777 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       ||_||_||_||_||_||_||_||_||_|""",
      "888888888 ERR"
    ),
    (""" _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       | _| _| _| _| _| _| _| _| _|""",
      "999999999 ERR"
    ),
    (""" _  _  _  _  _  _  _  _    
       || || || || || || || ||_   |
       ||_||_||_||_||_||_||_| _|  |""",
      "000000051"
    ),
    ("""    _  _  _  _  _  _     _ 
       ||_||_|| || ||_   |  |  | _ 
       |  | _||_||_||_|  |  |  | _|""",
      "49006771? ILL"
    ),
    ("""    _  _     _  _  _  _  _ 
       |  | _| _||_| _ |_   ||_||_|
       |  ||_  _|  | _||_|  ||_| _ """,
      "1234?678? ILL"
    ),
    (""" _  _     _  _        _  _ 
       ||_ |_ |_| _|  |  ||_||_||_ 
       ||_||_|  | _|  |  |  | _| _|""",
      "664371495 ERR"
    )
  )
  
  forAll (reprModelsAndExpectedResults) { (reprModel: String, expectedResult: String) => {
    it should s"be able to parse the representation of $expectedResult" in {
      val actualResult = Parser.parse(formatGlyphString(reprModel))
      actualResult shouldEqual expectedResult
    }
  }}
}
