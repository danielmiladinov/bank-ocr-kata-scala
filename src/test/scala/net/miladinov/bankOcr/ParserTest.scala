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
    ("description", "representation", "expectedResult"),
    ("All zeroes",
     """ _  _  _  _  _  _  _  _  _ 
       || || || || || || || || || |
       ||_||_||_||_||_||_||_||_||_|""",
      "000000000"
    ),
    ("All 1s, corrected to 711111111",
     """                           
       |  |  |  |  |  |  |  |  |  |
       |  |  |  |  |  |  |  |  |  |""",
      "711111111"
    ),
    ("All 2s, which isn't a valid account number",
     """ _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ """,
      "222222222 ERR"
    ),
    ("All 3s, which can be corrected to 333393333",
     """ _  _  _  _  _  _  _  _  _ 
       | _| _| _| _| _| _| _| _| _|
       | _| _| _| _| _| _| _| _| _|""",
      "333393333"
    ),
    ("All 4s, which isn't a valid account number",
     """                           
       ||_||_||_||_||_||_||_||_||_|
       |  |  |  |  |  |  |  |  |  |""",
      "444444444 ERR"
    ),
    ("All 5s, which is invalid and can be ambiguously corrected to either 555655555 or 559555555",
     """ _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       | _| _| _| _| _| _| _| _| _|""",
      "555555555 AMB ['555655555', '559555555']"
    ),
    ("All 6s, which is invalid and can be ambiguously corrected to either 666566666 or 686666666",
     """ _  _  _  _  _  _  _  _  _ 
       ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
       ||_||_||_||_||_||_||_||_||_|""",
      "666666666 AMB ['666566666', '686666666']"
    ),
    ("All 7s, which can be corrected to 777777177",
     """ _  _  _  _  _  _  _  _  _ 
       |  |  |  |  |  |  |  |  |  |
       |  |  |  |  |  |  |  |  |  |""",
      "777777177"
    ),
    ("All 8s, which is invalid and can be ambiguously corrected to either 888886888 or 888888880 or 888888988",
     """ _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       ||_||_||_||_||_||_||_||_||_|""",
      "888888888 AMB ['888886888', '888888880', '888888988']"
    ),
    ("All 9s, which is invalid and can be ambiguously corrected to either 899999999 or 993999999 or 999959999",
     """ _  _  _  _  _  _  _  _  _ 
       ||_||_||_||_||_||_||_||_||_|
       | _| _| _| _| _| _| _| _| _|""",
      "999999999 AMB ['899999999', '993999999', '999959999']"
    ),
    ("An accurate scan of 000000051",
     """ _  _  _  _  _  _  _  _    
       || || || || || || || ||_   |
       ||_||_||_||_||_||_||_| _|  |""",
      "000000051"
    ),
    ("A flawed scan of 000000051 which can be corrected",
     """ _     _  _  _  _  _  _    
       || || || || || || || ||_   |
       ||_||_||_||_||_||_||_| _|  |""",
      "000000051"
    ),
    ("A flawed scan of 49006771? where the last digit is illegible",
     """    _  _  _  _  _  _     _ 
       ||_||_|| || ||_   |  |  | _ 
       |  | _||_||_||_|  |  |  | _|""",
      "49006771? ILL"
    ),
    ("490067715 which is invalid and can be ambiguously corrected to 490067115 or 490067719 or 490867715",
     """    _  _  _  _  _  _     _ 
       ||_||_|| || ||_   |  |  ||_ 
       |  | _||_||_||_|  |  |  | _|""",
      "490067715 AMB ['490067115', '490067719', '490867715']"
    ),
    ("An accurate scan of 490867715",
     """    _  _  _  _  _  _     _ 
       ||_||_|| ||_||_   |  |  | _ 
       |  | _||_||_||_|  |  |  | _|""",
      "490867715"
    ),
    ("A flawed scan of 1234?678? containing multiple illegible characters",
     """    _  _     _  _  _  _  _ 
       |  | _| _||_| _ |_   ||_||_|
       |  ||_  _|  | _||_|  ||_| _ """,
      "1234?678? ILL"
    ),
    ("An accurate scan of 123456789",
     """    _  _     _  _  _  _  _ 
       | _| _| _||_||_ |_   ||_||_|
       |  ||_  _|  | _||_|  ||_| _|""",
      "123456789"
    ),
    ("An accurate scan of 664371495, which is invalid and can be corrected to 664371485",
     """ _  _     _  _        _  _ 
       ||_ |_ |_| _|  |  ||_||_||_ 
       ||_||_|  | _|  |  |  | _| _|""",
      "664371485"
    ),
    ("An accurate scan of 200800000",
     """ _  _  _  _  _  _  _  _  _ 
       | _|| || || || || || || || |
       ||_ |_||_||_||_||_||_||_||_|""",
      "200800000"
    )
  )
  
  forAll (reprModelsAndExpectedResults) { (description: String,  reprModel: String, expectedResult: String) => {
    it should s"be able to parse the representation of $description with an expected result of $expectedResult" in {
      val actualResult = Parser.parse(formatGlyphString(reprModel))
      actualResult shouldEqual expectedResult
    }
  }}
}
